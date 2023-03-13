(;; TODO better dummy cart that disables sound on startup
 ("atari2600" emulator: mame
  startup-args: ("a2600" "-cart" "mame-bridge/a2600_dummy.bin"))
 ("cbm8032" emulator: mame startup-args: ("cbm8032"))
 ("channelf" emulator: mame
  startup-args: ("channelf" "-cart" "mame-bridge/channelf_dummy.bin"))
 ("coco3" emulator: mame startup-args: ("coco3"))
 ("dragon32" emulator: mame startup-args: ("dragon32"))
 ("kc85_4" emulator: mame startup-args: ("kc85_4"))
 ("mc10" emulator: mame startup-args: ("mc10" "-ext" "ram"))
 ("mz700" emulator: mame startup-args: ("mz700"))
 ("spectrum48" emulator: mame startup-args: ("spectrum"))
 ("sorcerer" emulator: mame startup-args: ("sorcerer")))
