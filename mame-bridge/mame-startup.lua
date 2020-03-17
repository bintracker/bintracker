-- io.stdin:setvbuf'line'

local listener = emu.thread()
local started = false
local src = [[
   print("thread started")  -- works
   io.flush()

   local input = io.stdin:read() -- this blocks  -- which is technically fine...
   print("got input")   -- we do get here, but only we don't choke the thread before
   if input then
       io.stdout:write(input)
       io.stdout:write('\n')
       io.flush()
       return input
   end

   -- for line in io.stdin:lines() do
   --    io.stdout:write(line)
   --    io.stdout:write('\n')
   --    io.stdout:write("done")
   --    io.stdout:write('\n')
   --    io.flush()
   --    local exec_input = loadstring(line)
   --    exec_input()
   --    emu.pause()
   -- end
]]



emu.register_periodic(
   function()
      if listener.busy then
	 print "busy"
	 return
      elseif listener.yield then
	 print "yielded"
	 return
      elseif started then
	 local res = listener.result
	 print("got result:")
	 print(res)
	 local exec_input = loadstring(res)
	 exec_input()
      end
      listener:start(src)
      started = true
end)
