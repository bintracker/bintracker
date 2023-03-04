
((mame program-name: "mame"
       default-args: ("-w" "-skip_gameinfo" "-autoboot_script"
		      "mame-bridge/mame-startup.lua"
		      "-rompath" "roms"
		      "-uimodekey" "F12"
		      "-nomouse"
		      "-mouse_device" "none"
		      "-autoboot_delay" "0")))
