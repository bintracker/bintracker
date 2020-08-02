(;; TODO better dummy cart that disables sound on startup
 ("atari2600" emulator: "mame64"
  startup-args: ("a2600" "-cart" "mame-bridge/a2600_dummy.bin")
  loader-type: cart default-run-address: #xf000)
 ("channelf" emulator: "mame64"
  startup-args: ("channelf" "-cart" "mame-bridge/channelf_dummy.bin")
  pc-name: "PC0" loader-type: cart default-run-address: #x802)
 ("dragon32" emulator: "mame64" startup-args: ("dragon32"))
 ("kc85_4" emulator: "mame64" startup-args: ("kc85_4"))
 ("mz700" emulator: "mame64" startup-args: ("mz700"))
 ("spectrum48" emulator: "mame64" startup-args: ("spectrum"))
 ("sorcerer" emulator: "mame64" startup-args: ("sorcerer")))
