
((mame program-name: "3rdparty\\mame\\mame.exe"
       default-args: ("-w" "-skip_gameinfo" "-autoboot_script"
		      "mame-bridge/mame-startup.lua"
		      "-rompath" "roms"
		      "-nomouse"
		      "-mouse_device" "none"
		      "-autoboot_delay" "0")))
