-- This file is part of Bintracker.
-- Copyright (c) utz/irrlicht project 2019-2021
-- See LICENSE for license details.

-- io.stdin:setvbuf'line'

local listener = emu.thread()
local started = false

-- Ensure backwards compatibility with MAME <= 0.226
local machine_manager
if tonumber(emu.app_version()) >= 0.227 then
   machine_manager = manager.machine
else
   machine_manager = manager:machine()
end

-- Detect system's CPU device name.
local main_cpu = ":maincpu"
if machine_manager.devices[":f3"] ~= nil then
   -- using m6502 driver
   main_cpu = ":f3"
end

local print_machine_info = function ()
   print("System: ", emu.gamename())
   print("driver: ", emu.romname())
   print("\nMachine devices [machine_manager.devices]")
   for k,_ in pairs(machine_manager.devices) do print(k) end
   print("\nMachine options")
   -- appearantly this is the same as calling manager:options().entries
   for k,v in pairs(machine_manager:options().entries) do
      print(k, "=", v:value())
   end
   local cpu = machine_manager.devices[main_cpu]
   print("\nCPU State Registers\nState:")
   for k,v in pairs(cpu.state) do print(k, v.value) end
   -- print("\nSpaces:")
   -- for k,v in pairs(cpu.spaces) do print(k) end
   -- print("\nItems:")
   -- for k,v in pairs(cpu.items) do print(k) end
   print("\nMemory layout")
   for k,_ in pairs(cpu.spaces) do print(k) end
   if machine_manager.devices[":cartslot"] ~= nil then
      local cartslot = machine_manager.devices[":cartslot"]
      print("\nCartridge:");
      for k,_ in pairs(cartslot.spaces) do print(k) end
   end
   print("\nShares all:\n")
   for k,_ in pairs(machine_manager:memory().shares) do print (k) end
   print("\nRegions all:\n")
   for k,_ in pairs(machine_manager:memory().regions) do print (k) end
   -- print("\nRegions maincpu")
   -- local regions = manager:machine():memory().regions[":maincpu"]
   -- print((tostring(regions)))
   -- print("cartslot state")
   -- for k,v in pairs(cartslot.state) do print(k) end
end

local pc_name = "PC"
local loader_type = 0 -- 0 = RAM, 1 = cartridge
local default_run_address = false

local machine_set_pc = function (addr)
   machine_manager.devices[main_cpu].state[pc_name].value = tonumber(addr)
end

-- unfreeze Z80 emulation after halt instruction
local machine_unhalt = function ()
   if machine_manager.devices[main_cpu].state["HALT"] ~= nil then
      machine_manager.devices[main_cpu].state["HALT"].value = 0
   end
end

local machine_load_bin = function (addr, data)
   local datatbl = {string.byte(data, 1, #data)}
   local mem
   local local_addr = addr
   if loader_type == 0 then
      mem = machine_manager.devices[main_cpu].spaces["program"]
   else
      do
	 mem = machine_manager:memory().regions[":cartslot:cart:rom"]
	 local_addr = 0
      end
   end
   for i = 1, #datatbl do
      -- print("write at ", addr, ": ", (tostring(datatbl[i])))
      mem:write_u8(local_addr, datatbl[i])
      -- print("read back: ", (tostring(mem:read_u8(addr))))
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
   -- print(res)
   return tonumber(res)
end

local get_data_arg = function (argstr)
   while string.sub(argstr, 1, 1) ~= "%" do
      argstr = string.sub(argstr, 2)
   end
   -- print(string.sub(argstr, 2))
   return string.sub(argstr, 2)
end

local machine_run_bin = function (argstr)
   local addr = get_numeric_arg(argstr)
   -- local data = get_data_arg(argstr)
   emu.pause()
   machine_load_bin(addr, base64_decode(get_data_arg(argstr)))
   if default_run_address then
      machine_set_pc(default_run_address)
   else
      machine_set_pc(addr)
   end
   machine_unhalt()
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
   ["d"] = function (argstr) default_run_address = tonumber(argstr) end,
   ["l"] = function (argstr) loader_type = tonumber(argstr) end,
   ["n"] = function (argstr) pc_name = argstr end,
   ["q"] = function () machine_manager:exit() end,
   ["p"] = emu.pause,
   ["r"] = machine_reset,
   ["s"] = machine_set_pc,
   ["u"] = emu.unpause,
   ["x"] = function (argstr) loadstring(argstr)() end
}

-- Attempt to destructure and run the remote command `cmd`. Takes the first
-- letter of `cmd` as key and looks up the associated function in
-- `remote_commands`. When successful, runs the function with the remainder of
-- `cmd` as argument.
local dispatch_remote_command = function(cmd)
   -- print("got command: ", cmd)
   local exec_cmd = remote_commands[string.sub(cmd, 1, 1)]
   if exec_cmd then exec_cmd(string.sub(cmd, 2)) end
end

-- -- not implemented yet in MAME 0.209?
-- emu.register_mandatory_file_manager_override(
--    function()
--       print("have mandatory file callback")
--    end
-- )

-- Register a period callback from the main emulation thread. On first run, it
-- starts a thread that listens to stdin, and returns the received input once it
-- receives a newline. The callback procedure attempts to run the input from the
-- listener as a remote command, then restarts the listener thread.
emu.register_periodic(
   function()
      if listener.busy then
	 return
      elseif listener.yield then
	 return
      elseif started then
	 dispatch_remote_command(listener.result)
      end
      listener:start([[ return io.stdin:read() ]])
      started = true
   end
)
