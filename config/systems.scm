(;; TODO better dummy cart that disables sound on startup
 ("atari2600" emulator: mame
  startup-args: ("a2600" "-cart" "mame-bridge/a2600_dummy.bin")
  loader-type: cart default-run-address: #xf000)
 ("cbm8032" emulator: mame
  startup-args: ("cbm8032") default-run-address: #x40d)
 ("channelf" emulator: mame
  startup-args: ("channelf" "-cart" "mame-bridge/channelf_dummy.bin")
  pc-name: "PC0" loader-type: cart default-run-address: #x802)
 ("dragon32" emulator: mame startup-args: ("dragon32"))
 ("kc85_4" emulator: mame startup-args: ("kc85_4"))
 ("mz700" emulator: mame startup-args: ("mz700"))
 ("spectrum48" emulator: mame startup-args: ("spectrum"))
 ("sorcerer" emulator: mame startup-args: ("sorcerer")))
