-- This file is part of Bintracker.
-- Copyright (c) utz/irrlicht project 2019-2020
-- See LICENSE for license details.

-- io.stdin:setvbuf'line'

local listener = emu.thread()
local started = false

local print_machine_info = function ()
   print("System: ", emu.gamename())
   print("driver: ", emu.romname())
   print("\nMachine devices [manager:machine().devices]")
   for k,v in pairs(manager:machine().devices) do print(k) end
   print("\nMachine options")
   -- appearantly this is the same as calling manager:options().entries
   for k,v in pairs(manager:machine():options().entries) do
      print(k, "=", v:value())
   end
   local cpu = manager:machine().devices[":maincpu"]
   print("\nCPU State Registers")
   for k,v in pairs(cpu.state) do print(k) end
   print("\nMemory layout")
   for k,v in pairs(cpu.spaces) do print(k) end
end

-- Table of remote commands that Bintracker may send. The following commands
-- are recognized:
-- q - Quit emulator
-- p - Pause emulator
-- u - Unpause emulator
-- x argstr - eXecute argstr as code
local remote_commands = {
   ["i"] = print_machine_info,
   ["q"] = function () manager:machine():exit() end,
   ["p"] = function () emu.pause() end,
   ["u"] = function () emu.unpause() end,
   ["x"] = function (argstr) loadstring(argstr)() end
}

-- Attempt to destructure and run the remote command `cmd`. Takes the first
-- letter of `cmd` as key and looks up the associated function in
-- `remote_commands`. When successful, runs the function with the remainder of
-- `cmd` as argument.
local dispatch_remote_command = function(cmd)
   print("got command: ", cmd)
   local exec_cmd = remote_commands[string.sub(cmd, 1, 1)]
   if exec_cmd then exec_cmd(string.sub(cmd, 2)) end
end

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
end)
