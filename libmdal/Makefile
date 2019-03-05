mdal: mdal.scm note-table helpers globals command core
	csc -s -d1 mdal.scm -j mdal
	csc -s mdal.import.scm -d0

note-table: utils/md-note-table.scm
	csc -s -d1 utils/md-note-table.scm -j md-note-table
	mv utils/md-note-table.so ./
	csc -s md-note-table.import.scm -d0

helpers: md-helpers.scm
	csc -s -d1 md-helpers.scm -j md-helpers
	csc -s md-helpers.import.scm -d0

globals: helpers md-globals.scm
	csc -s -d1 md-globals.scm -j md-globals
	csc -s md-globals.import.scm -d0

command: helpers md-command.scm
	csc -s -d1 md-command.scm -j md-command
	csc -s md-command.import.scm -d0

# config: helpers globals command md-config.scm
# 	csc -s -d1 md-config.scm -j md-config
# 	csc -s md-config.import.scm -d0

core: mdal-core.scm note-table helpers globals command
	csc -s -d1 mdal-core.scm -j mdal-core
	csc -s mdal-core.import.scm -d0

docs: mdal-core.scm md-helpers.scm md-command.scm utils/md-note-table.scm
	scm2wiki -i mdal-core.scm -o docs/generated/mdal-core.md -m
	scm2wiki -i md-helpers.scm -o docs/generated/md-helpers.md -m
	scm2wiki -i md-command.scm -o docs/generated/md-command.md -m
	scm2wiki -i utils/md-note-table.scm -o docs/generated/util/md-note-table.md -m

.PHONY: clean
clean:
	rm *.so *.import.scm
