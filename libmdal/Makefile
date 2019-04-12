CSC = csc
DOCGEN = scm2wiki
DOCS = mdal.md md-config.md md-helpers.md md-types.md md-parser.md\
 md-command.md utils/md-note-table.md
LIBFLAGS = -s -d1
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0
BASEDEPS = md-helpers.import.so md-types.import.so

# build libmdal
mdal: mdal.scm $(BASEDEPS) md-config.import.so md-parser.import.so
	$(CSC) $(LIBFLAGS) mdal.scm -j mdal
	$(CSC) $(IMPORTFLAGS) mdal.import.scm

md-helpers.so: md-helpers.scm
	$(CSC) $(LIBFLAGS) md-helpers.scm -j md-helpers

md-helpers.import.so: md-helpers.so
	$(CSC) $(IMPORTFLAGS) md-helpers.import.scm

md-types.so: md-helpers.import.so md-types.scm
	$(CSC) $(LIBFLAGS) md-types.scm -j md-types

md-types.import.so: md-types.so
	$(CSC) $(IMPORTFLAGS) md-types.import.scm

md-note-table.so: utils/md-note-table.scm
	$(CSC) $(LIBFLAGS) utils/md-note-table.scm -j md-note-table
	mv utils/md-note-table.so ./

md-note-table.import.so: md-note-table.so
	$(CSC) $(IMPORTFLAGS) md-note-table.import.scm

md-command.so: md-helpers.import.so md-command.scm
	$(CSC) $(LIBFLAGS) md-command.scm -j md-command

md-command.import.so: md-command.so
	$(CSC) $(IMPORTFLAGS) md-command.import.scm

md-config.so: md-config.scm $(BASEDEPS) md-command.import.so\
 md-note-table.import.so
	$(CSC) $(LIBFLAGS) md-config.scm -j md-config

md-config.import.so: md-config.so
	$(CSC) $(IMPORTFLAGS) md-config.import.scm

md-parser.so: md-parser.scm $(BASEDEPS) md-config.import.so
	$(CSC) $(LIBFLAGS) md-parser.scm -j md-parser

md-parser.import.so: md-parser.so
	$(CSC) $(IMPORTFLAGS) md-parser.import.scm

# generate documentation
%.md: %.scm
	$(DOCGEN) -i $< -o docs/generated/$@ -m

mdal.md: mdal.scm
md-config.md: md-config.scm
md-helpers.md: md-helpers.scm
md-types.md: md-types.scm
md-parser.md: md-parser.scm
md-command.md: md-command.scm
md-note-table.md: utils/md-note-table.scm

docs: $(DOCS)

# run unit tests
.PHONY: run-tests
run-tests: mdal
	cp -t ./ unittests/unittests.scm && csi unittests.scm -e
	rm unittests.scm

.PHONY: clean
clean:
	-rm *.so *.import.scm docs/generated/*.md docs/generated/utils/*.md
