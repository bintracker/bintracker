-- This file is part of Bintracker.
-- Copyright (c) utz/irrlicht project 2019-2023
-- See LICENSE for license details.

-- Set up tcp server
local tcp_in = emu.file("", 'rwc')
tcp_in:open("socket.127.0.0.1:4321")

local rprint = function (str)
   tcp_in:write(str .. "\n")
end


-- Enumerated loader types. See `machine_load_bin` below.
local loader_types = { ram = 0, cart = 1 }

-- This table holds information on machine specific emulation features.
-- The following defaults are implicit:
--   cpu_name = ":main_cpu"
--   pc_name = "PC"
--   loader_type = loader_types["ram"]
--   default_run_address = nil
--   post_load_actions = nil
--   init_tap_ignore = nil
--   init_tap = nil
local machine_features = {
   a2600 = {
      loader_type = loader_types["cart"],
      default_run_address = 0xf000
   },
   cbm8032 = {
      cpu_name = ":f3",
      default_run_address = 0x40d
   },
   channelf = {
      pc_name = "PC0",
      loader_type = loader_types["cart"],
      default_run_address = 0x802,
      post_load_actions = function ()
	 manager.machine:soft_reset()
      end
   },
   coco3 = {
      init_tap_ignore = 5,
      -- alternatives: 0x152, 0x7f21, 0x7f23
      init_tap = 0x94
   },
   dragon32 = {
      init_tap_ignore = 6,
      -- alternatives: 0xff02
      init_tap = 0x8f
   },
   kc85_4 = {},
   mc10 = {},
   mz700 = {
      post_load_actions = function ()
	 -- set stack pointer to a safe address
	 -- manager.machine.devices[":maincpu"].state["SP"].value = 0xfffe
	 -- unfreeze Z80 emulation after halt instruction on newer MAME versions
	 if manager.machine.devices[":maincpu"].state["HALT"] ~= nil then
	    manager.machine.devices[":maincpu"].state["HALT"].value = 0
	 end
      end
   },
   spectrum = {
      post_load_actions = function ()
	 -- clear pending interrupts
	 manager.machine.devices[":maincpu"].state["IM"].value = 1
	 manager.machine.devices[":maincpu"].state["IFF1"].value = 0
	 manager.machine.devices[":maincpu"].state["IFF2"].value = 0
	 -- set stack pointer to a safe address
	 manager.machine.devices[":maincpu"].state["SP"].value = 0xfffe
	 -- unfreeze Z80 emulation after halt instruction on newer MAME versions
	 if manager.machine.devices[":maincpu"].state["HALT"] ~= nil then
	    manager.machine.devices[":maincpu"].state["HALT"].value = 0
	 end
      end
   },
   sol20 = {},
   sorcerer = {
      post_load_actions = function ()
	 -- unfreeze Z80 emulation after halt instruction on newer MAME versions
	 if manager.machine.devices[":maincpu"].state["HALT"] ~= nil then
	    manager.machine.devices[":maincpu"].state["HALT"].value = 0
	 end
      end
   }
}

-- initialize machine_features implicit defaults for emulated machine
local machine_specific = machine_features[emu.romname()]
if machine_specific.cpu_name == nil then
   machine_specific.cpu_name = ":maincpu"
end
if machine_specific.pc_name == nil then
   machine_specific.pc_name = "PC"
end
if machine_specific.loader_type == nil then
   machine_specific.loader_type = loader_types["ram"]
end

-- Extract machine specific vars for faster access
local machine_cpu = manager.machine.devices[machine_specific.cpu_name]
local machine_pc = machine_cpu.state[machine_specific.pc_name]
local loader_type = machine_specific.loader_type
local default_run_address = machine_specific.default_run_address
local post_load_actions = machine_specific.post_load_actions


local print_machine_info = function ()
   rprint("System: " .. emu.gamename())
   rprint("driver: " .. emu.romname())
   rprint("\nMachine devices [manager.machine.devices]")
   for k,_ in pairs(manager.machine.devices) do rprint(k) end
   rprint("\nMachine options")
   for k,v in pairs(manager.machine.options.entries) do
      rprint(k .. "=" .. tostring(v:value()))
   end
   rprint("\nCPU State Registers\nState:")
   for k,v in pairs(machine_cpu.state) do rprint(k .. v.value) end
   -- rprint("\nSpaces:")
   -- for k,v in pairs(machine_cpu.spaces) do rprint(k) end
   -- rprint("\nItems:")
   -- for k,v in pairs(machine_cpu.items) do rprint(k) end
   rprint("\nMemory layout")
   for k,_ in pairs(machine_cpu.spaces) do rprint(k) end
   if manager.machine.devices[":cartslot"] ~= nil then
      local cartslot = manager.machine.devices[":cartslot"]
      rprint("\nCartridge:");
      for k,_ in pairs(cartslot.spaces) do rprint(k) end
   end
   rprint("\nShares all:\n")
   for k,_ in pairs(manager.machine.memory.shares) do rprint (k) end
   rprint("\nRegions all:\n")
   for k,_ in pairs(manager.machine.memory.regions) do rprint (k) end
end


local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

local base64_decode = function (data)
    data = string.gsub(data, '[^'..b..'=]', '')
    return (data:gsub('.', function(x)
        if (x == '=') then return '' end
        local r,f = '' , (b:find(x)-1)
        for i = 6, 1, -1 do r = r..(f % 2^i - f% 2^(i-1) > 0 and '1' or '0') end
        return r;
    end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
        if (#x ~= 8) then return '' end
        local c=0
        for i = 1, 8 do c = c + (x:sub(i ,i) == '1' and 2^(8-i) or 0) end
            return string.char(c)
    end))
end


-- Extract arguments from a remote command string, where arguments are separated
-- with a percent sign, and the initial command prefix is dropped.
local get_args = function (argstr)
   local args = {}
   local idx = 1
   for str in string.gmatch(argstr, "([^%%]+)") do
      args[idx] = str
      idx = idx + 1
   end
   return args
end


-- Write tap may be triggered multiple times on startup. Setting this variable
-- to N means N tap triggers will be ignored before the tap callback runs.
-- Setting machine_specific.init_tap_ignore will set this value.
local tap_count = 0
local active_tap = false

local make_tap_callback = function (send_msg)
   return function (offset, data, read_mask)
      -- rprint("Machine state: " .. manager.machine.phase)
      if tap_count == 0 then
	 do
	    -- rprint("write tap callback called, offset: "
	    -- 	   .. tostring(offset)
	    -- 	   .. ", data: " .. tostring(data) ..
	    -- 	   ", readmask " .. tostring(read_mask))
	    emu.pause()
	    -- active_tap:remove()
	    -- TODO skip N writes again, do this properly (eg. check if N!=0)
	    -- the below doesn't work because # of skipped taps seems to be random, or at least
	    -- threading makes it appear so
	    -- seems to work anyway without it
	    -- tap_count = machine_specific.init_tap_ignore
	    if send_msg ~= nil then rprint(send_msg) end
	 end
      else
	 do
	    -- rprint("ignoring write tap")
	    tap_count = tap_count - 1
	 end
      end
   end
end

local set_tap = function (addr, send_msg)
   local paused = manager.machine.paused
   emu.pause()
   -- rprint("set_tap " .. tostring(addr) .. send_msg)
   if active_tap == true then active_tap:remove() end
   active_tap = machine_cpu.spaces["program"]:install_write_tap(
      addr, addr, "active_tap_cb", make_tap_callback(send_msg))
   if paused == false then emu.unpause() end
   -- rprint("set_tap done")
end

local install_tap = function(argstr)
   -- rprint("installing write tap at " .. argstr)
   local args = get_args(argstr)
   local addr = tonumber(args[1])
   local send_msg = args[2]
   -- rprint("addr = " .. tostring(addr))
   set_tap(addr, send_msg)
end


local machine_set_pc = function (addr)
   machine_pc.value = tonumber(addr)
end

local machine_load_bin = function (addr, data)
   local datatbl = {string.byte(data, 1, #data)}
   local mem
   local local_addr = addr
   if loader_type == loader_types["ram"] then
      mem = machine_cpu.spaces["program"]
   else
      do
	 mem = manager.machine.memory.regions[":cartslot:cart:rom"]
	 local_addr = 0
      end
   end
   for i = 1, #datatbl do
      mem:write_u8(local_addr, datatbl[i])
      local_addr = local_addr + 1
   end
end

local machine_run_bin_impl = function (addr, data)
   machine_load_bin(addr, data)
   if default_run_address ~= nil then
      machine_set_pc(default_run_address)
   else
      machine_set_pc(addr)
   end
   if post_load_actions ~= nil then post_load_actions() end
end

local machine_run_bin = function (argstr)
   emu.pause()
   local args = get_args(argstr)
   local addr = tonumber(args[1])
   local data = base64_decode(args[2])
   machine_run_bin_impl(addr, data)
   -- machine_load_bin(addr, data)
   -- if default_run_address ~= nil then
   --    machine_set_pc(default_run_address)
   -- else
   --    machine_set_pc(addr)
   -- end
   -- if post_load_actions ~= nil then post_load_actions() end
   emu.unpause()
end

local live_update = function (argstr)
   emu.pause()
   rprint("live_update " .. argstr)
   local args = get_args(argstr)
   set_tap(tonumber(args[2]), "&rd")
   machine_run_bin_impl(tonumber(args[1]), base64_decode(args[3]))
   emu.unpause()
end

local machine_reset = function (reset_type)
   if string.sub(reset_type, 1, 1) == "h" then
      manager.machine:hard_reset()
   elseif string.sub(reset_type, 1, 1) == "s" then
      manager.machine:soft_reset()
   end
end

local exit_emul = function ()
   -- pass-through handlers must be explicitly removed on exit from session
   if active_tap == true then active_tap:remove() end
   manager.machine:exit()
end


-- Table of remote commands that Bintracker may send. The following commands
-- are recognized:
-- q - Quit emulator
-- p - Pause emulator
-- u - Unpause emulator
-- x argstr - eXecute argstr as code
local remote_commands = {
   ["b"] = machine_run_bin,
   ["i"] = print_machine_info,
   ["l"] = live_update,
   ["q"] = exit_emul,
   ["p"] = emu.pause,
   ["r"] = machine_reset,
   ["s"] = machine_set_pc,
   ["u"] = emu.unpause,
   ["t"] = install_tap,
   ["x"] = function (argstr) loadstring(argstr)() end
}

-- Initialize write tap on startup if required by machine.
if machine_specific.init_tap_ignore ~= nil then
   do
      tap_count = machine_specific.init_tap_ignore
      set_tap(machine_specific.init_tap, "testtap")
   end
end


-- Listen on tcp port and dispatch incoming remote commands.
emu.register_periodic(
   function()
      local data = ""

      repeat
	 local read = tcp_in:read(100)
	 data = data .. read
      until #read == 0

      if #data == 0 then return end

      -- dispatch remote command
      -- rprint("got command: " .. data)
      local exec_cmd = remote_commands[string.sub(data, 1, 1)]
      if exec_cmd then exec_cmd(string.sub(data, 2)) end
   end
)
