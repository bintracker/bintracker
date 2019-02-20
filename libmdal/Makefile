mdal: note-table helpers command

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

.PHONY: clean
clean:
	rm *.so *.import.scm
