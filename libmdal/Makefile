mdal: mdal.scm note-table helpers command mdal-core
	csc -s -d1 mdal.scm -j mdal
	csc -s mdal.import.scm -d0

note-table: utils/md-note-table.scm
	csc -s -d1 utils/md-note-table.scm -j md-note-table
	mv utils/md-note-table.so ./
	csc -s md-note-table.import.scm -d0

helpers: md-helpers.scm
	csc -s -d1 md-helpers.scm -j md-helpers
	csc -s md-helpers.import.scm -d0

command: helpers md-command.scm
	csc -s -d1 md-command.scm -j md-command
	csc -s md-command.import.scm -d0

mdal-core: mdal-core.scm note-table helpers command
	csc -s -d1 mdal-core.scm -j mdal-core
	csc -s mdal-core.import.scm -d0

.PHONY: clean
clean:
	rm *.so *.import.scm
