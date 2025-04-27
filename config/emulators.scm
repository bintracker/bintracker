
((mame program-name: "mame"
       default-args: ("-w" "-skip_gameinfo" "-autoboot_script"
		      "mame-bridge/mame-startup.lua"
		      "-rompath" "roms"
		      "-uimodekey" "F12"
		      "-nomouse"
		      "-mouse_device" "none"
		      "-autoboot_delay" "0"))

 ;; (xroar adapter: make-gdb-instance
 ;; 	program-name: "xroar"
 ;; 	default-args: ("-ao" "alsa" "-gdb" "-gdb-port" "4322" "-v" "3" "-debug-gdb" "-1"))
 )
