-- This file is part of Bintracker.
-- Copyright (c) utz/irrlicht project 2019-2023
-- See LICENSE for license details.

-- Set up tcp server
local tcp_in = emu.file("", 'rwc')
tcp_in:open("socket.127.0.0.1:4321")

local rprint = function (str)
   tcp_in:write(str .. "\n")
end

-- Ensure backwards compatibility with MAME <= 0.226
local machine_manager
if tonumber(emu.app_version()) >= 0.227 then
   machine_manager = manager.machine
else
   machine_manager = manager:machine()
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
local machine_features = {
   a2600 = {loader_type = loader_types["cart"],
	    default_run_address = 0xf000},
   cbm8032 = {cpu_name = ":f3",
	      default_run_address = 0x40d},
   channelf = {pc_name = "PC0",
	       loader_type = loader_types["cart"],
	       default_run_address = 0x802,
	       post_load_actions = function ()
		  machine_manager:soft_reset()
	       end
   },
   coco3 = {},
   dragon32 = {},
   kc85_4 = {},
   mz700 = {},
   spectrum = {
      post_load_actions = function ()
	 -- set stack pointer to a safe address
	 machine_manager.devices[":maincpu"].state["SP"].value = 0xfffe
	 -- unfreeze Z80 emulation after halt instruction on newer MAME versions
	 if machine_manager.devices[":maincpu"].state["HALT"] ~= nil then
	    machine_manager.devices[":maincpu"].state["HALT"].value = 0
	 end
      end
   },
   sorcerer = {
      post_load_actions = function ()
	 -- unfreeze Z80 emulation after halt instruction on newer MAME versions
	 if machine_manager.devices[":maincpu"].state["HALT"] ~= nil then
	    machine_manager.devices[":maincpu"].state["HALT"].value = 0
	 end
      end
   }
}

-- initialize machine_features implicit defaults for emulated machine
local machine_specific = machine_features[emu.romname()]
if machine_specific.cpu_name == nil then
   machine_specific.cpu_name = ":maincpu"
end
if machine_specific.pc_name == nil then machine_specific.pc_name = "PC" end
if machine_specific.loader_type == nil then
      machine_specific.loader_type = loader_types["ram"]
end

-- Extract machine specific vars for faster access
local machine_cpu = machine_manager.devices[machine_specific.cpu_name]
local machine_pc = machine_cpu.state[machine_specific.pc_name]
local loader_type = machine_specific.loader_type
local default_run_address = machine_specific.default_run_address
local post_load_actions = machine_specific.post_load_actions

local print_machine_info = function ()
   rprint("System: " .. emu.gamename())
   rprint("driver: " .. emu.romname())
   rprint("\nMachine devices [machine_manager.devices]")
   for k,_ in pairs(machine_manager.devices) do rprint(k) end
   rprint("\nMachine options")
   for k,v in pairs(machine_manager.options.entries) do
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
   if machine_manager.devices[":cartslot"] ~= nil then
      local cartslot = machine_manager.devices[":cartslot"]
      rprint("\nCartridge:");
      for k,_ in pairs(cartslot.spaces) do rprint(k) end
   end
   rprint("\nShares all:\n")
   for k,_ in pairs(machine_manager.memory.shares) do rprint (k) end
   rprint("\nRegions all:\n")
   for k,_ in pairs(machine_manager.memory.regions) do rprint (k) end
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
	 mem = machine_manager.memory.regions[":cartslot:cart:rom"]
	 local_addr = 0
      end
   end
   for i = 1, #datatbl do
      mem:write_u8(local_addr, datatbl[i])
      -- print("write: ", (tostring(datatbl[i])),
      -- 	    ", read: ", (tostring(mem:read_u8(local_addr))))
      local_addr = local_addr + 1
   end
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

-- extract a numeric argument from a remote command string. The numeric argument
-- must be at the beginning of the string, and must be terminated with a `%`
-- character.
local get_numeric_arg = function (argstr)
   local res = ""
   while string.sub(argstr, 1, 1) ~= "%" do
      res = res..string.sub(argstr, 1, 1)
      argstr = string.sub(argstr, 2)
   end
   return tonumber(res)
end

local get_data_arg = function (argstr)
   while string.sub(argstr, 1, 1) ~= "%" do
      argstr = string.sub(argstr, 2)
   end
   return string.sub(argstr, 2)
end

local machine_run_bin = function (argstr)
   local addr = get_numeric_arg(argstr)
   emu.pause()
   machine_load_bin(addr, base64_decode(get_data_arg(argstr)))
   if default_run_address ~= nil then
      machine_set_pc(default_run_address)
   else
      machine_set_pc(addr)
   end
   if post_load_actions ~= nil then post_load_actions() end
   emu.unpause()
end

local machine_reset = function (reset_type)
   if reset_type == "h" then
      machine_manager:hard_reset()
   elseif reset_type == "s" then
      machine_manager:soft_reset()
   end
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
   ["q"] = function () machine_manager:exit() end,
   ["p"] = emu.pause,
   ["r"] = machine_reset,
   ["s"] = machine_set_pc,
   ["u"] = emu.unpause,
   ["x"] = function (argstr) loadstring(argstr)() end
}

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
