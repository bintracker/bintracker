CSC = csc
DOCGEN = scm2wiki
DOCS = bintracker-core.md bt-gui.md bt-gui-lolevel.md bt-state.md bt-types.md\
 bt-db.md bt-emulation.md
MDALDOCS = mdal.md md-config.md md-helpers.md md-types.md md-parser.md\
 md-command.md schemta.md utils/md-note-table.md
BASEDEPS = md-helpers.import.so md-types.import.so
LIBFLAGS = -s -d3 #-profile-name $@.PROFILE
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0
MDAL_SRC = ../libmdal/
BT_SRC = ../
ifdef ETAGS
 DO_TAGS = TAGS
endif

# Might need to use csc -compile-syntax in places. See:
# https://lists.nongnu.org/archive/html/chicken-users/2017-08/msg00004.html

bintracker: $(BT_SRC)bintracker.scm bintracker-core.import.so\
mame-bridge/mame-startup.lua config roms plugins resources
	$(CSC) $< -d3 -O2 -compile-syntax -o $@

# build bintracker-core
bintracker-core.so: $(BT_SRC)bintracker-core.scm bt-state.import.so\
 bt-types.import.so bt-gui.import.so bt-db.import.so bt-emulation.import.so\
 mdal.so $(DO_TAGS)
	$(CSC) $(LIBFLAGS) $< -j bintracker-core -o $@
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm

bintracker-core.import.so: bintracker-core.so
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm

bt-types.so: $(BT_SRC)bt-types.scm
	$(CSC) $(LIBFLAGS) $< -j bt-types -o $@

bt-types.import.so: bt-types.so
	$(CSC) $(IMPORTFLAGS) bt-types.import.scm

bt-state.so: $(BT_SRC)bt-state.scm bt-types.import.so bt-db.import.so\
 bt-emulation.import.so mdal.so
	$(CSC) $(LIBFLAGS) $< -j bt-state -o $@

bt-state.import.so: bt-state.so
	$(CSC) $(IMPORTFLAGS) bt-state.import.scm

bt-db.so: $(BT_SRC)bt-db.scm mdal.so
	$(CSC) $(LIBFLAGS) $< -j bt-db -o $@

bt-db.import.so: bt-db.so
	$(CSC) $(IMPORTFLAGS) bt-db.import.scm

bt-emulation.so: $(BT_SRC)bt-emulation.scm
	$(CSC) $(LIBFLAGS) $< -j bt-emulation -o $@

bt-emulation.import.so: bt-emulation.so
	$(CSC) $(IMPORTFLAGS) bt-emulation.import.scm

bt-gui.so: $(BT_SRC)bt-gui.scm bt-state.import.so bt-types.import.so bt-db.import.so\
 bt-emulation.import.so bt-gui-lolevel.import.so mdal.so
	$(CSC) $(LIBFLAGS) $< -j bt-gui -o $@

bt-gui.import.so: bt-gui.so
	$(CSC) $(IMPORTFLAGS) bt-gui.import.scm

bt-gui-lolevel.so: $(BT_SRC)bt-gui-lolevel.scm bt-state.import.so bt-types.import.so\
 bt-emulation.import.so
	$(CSC) $(LIBFLAGS) $< -j bt-gui-lolevel -o $@

bt-gui-lolevel.import.so: bt-gui-lolevel.so
	$(CSC) $(IMPORTFLAGS) bt-gui-lolevel.import.scm

# %.md: %.scm
# 	$(DOCGEN) -i $< -o docs/generated/$@

# bintracker-core.md: bintracker-core.scm
# bt-gui.md: bt-gui.scm
# bt-gui-lolevel.md: bt-gui-lolevel.scm
# bt-state.md: bt-state.scm
# bt-types.md: bt-types.scm
# bt-emulation.md: bt-emulation.scm
# bt-db.md: bt-db.scm

# docs: $(DOCS)
# 	$(MAKE) docs -C libmdal
# 	mkdir -p docs/libmdal
# 	cp -r libmdal/docs/* docs/libmdal/
# 	mkdocs build

mdal.so: $(MDAL_SRC)mdal.scm $(BASEDEPS) md-config.import.so\
 md-parser.import.so schemta.import.so
	$(CSC) $(LIBFLAGS) $< -j mdal -o $@
	$(CSC) $(IMPORTFLAGS) mdal.import.scm

md-helpers.so: $(MDAL_SRC)md-helpers.scm
	$(CSC) $(LIBFLAGS) $< -j md-helpers -o $@

md-helpers.import.so: md-helpers.so
	$(CSC) $(IMPORTFLAGS) md-helpers.import.scm

md-types.so: $(MDAL_SRC)md-types.scm md-helpers.import.so
	$(CSC) $(LIBFLAGS) $< -j md-types -o $@

md-types.import.so: md-types.so
	$(CSC) $(IMPORTFLAGS) md-types.import.scm

md-note-table.so: $(MDAL_SRC)utils/md-note-table.scm
	$(CSC) $(LIBFLAGS) $< -j md-note-table -o $@

md-note-table.import.so: md-note-table.so
	$(CSC) $(IMPORTFLAGS) md-note-table.import.scm

md-command.so: $(MDAL_SRC)md-command.scm md-helpers.import.so
	$(CSC) $(LIBFLAGS) $< -j md-command -o $@

md-command.import.so: md-command.so
	$(CSC) $(IMPORTFLAGS) md-command.import.scm

md-config.so: $(MDAL_SRC)md-config.scm $(BASEDEPS) md-command.import.so\
 md-note-table.import.so schemta.import.so
	$(CSC) $(LIBFLAGS) $< -j md-config -o $@

md-config.import.so: md-config.so
	$(CSC) $(IMPORTFLAGS) md-config.import.scm

md-parser.so: $(MDAL_SRC)md-parser.scm $(BASEDEPS) md-config.import.so
	$(CSC) $(LIBFLAGS) $< -j md-parser -o $@

md-parser.import.so: md-parser.so
	$(CSC) $(IMPORTFLAGS) md-parser.import.scm

schemta.so: $(MDAL_SRC)schemta.scm
	$(CSC) $(LIBFLAGS) $< -j schemta -o $@

schemta.import.so: schemta.so
	$(CSC) $(IMPORTFLAGS) schemta.import.scm

# # generate documentation
# %.md: %.scm
# 	$(DOCGEN) -i $< -o docs/generated/$@ -m

# mdal.md: mdal.scm
# md-config.md: md-config.scm
# md-helpers.md: md-helpers.scm
# md-types.md: md-types.scm
# md-parser.md: md-parser.scm
# md-command.md: md-command.scm
# md-note-table.md: utils/md-note-table.scm
# schemta.md: schemta.scm

# docs: $(DOCS)

# Generate ETags file for Emacs
TAGS: $(BT_SRC)bt-types.scm\
 $(BT_SRC)bt-state.scm\
 $(BT_SRC)bt-gui-lolevel.scm\
 $(BT_SRC)bt-gui.scm\
 $(BT_SRC)bt-db.scm\
 $(BT_SRC)bt-emulation.scm\
 $(BT_SRC)bintracker-core.scm\
 $(MDAL_SRC)schemta.scm\
 $(MDAL_SRC)md-parser.scm\
 $(MDAL_SRC)md-config.scm\
 $(MDAL_SRC)md-command.scm\
 $(MDAL_SRC)utils/md-note-table.scm\
 $(MDAL_SRC)md-types.scm\
 $(MDAL_SRC)md-helpers.scm\
 $(MDAL_SRC)mdal.scm
	cd .. && etags -r '"  (def.*? "' bt-types.scm bt-state.scm\
 bt-gui-lolevel.scm bt-gui.scm bt-db.scm bt-emulation.scm bintracker-core.scm\
 libmdal/schemta.scm libmdal/md-parser.scm libmdal/md-config.scm\
 libmdal/md-command.scm libmdal/md-types.scm libmdal/md-helpers.scm\
 libmdal/utils/md-note-table.scm libmdal/mdal.scm

mame-bridge/mame-startup.lua: $(BT_SRC)mame-bridge/mame-startup.lua
	mkdir -p mame-bridge
	cp $< mame-bridge/

.PHONY: config
config:
	cp -r $(BT_SRC)config/ .
	-rm -r config/*~

.PHONY: docs
docs:
	cp -r $(BT_SRC)docs/ .

.PHONY: roms
roms:
	cp -r $(BT_SRC)roms/ .

.PHONY: plugins
plugins:
	cp -r $(BT_SRC)plugins .

.PHONY: resources
resources:
	mkdir -p resources/
	cp -r $(BT_SRC)resources/icons resources/

# run unit tests
.PHONY: run-tests
run-tests: mdal.so
	cp -t ./ $(MDAL_SRC)unittests/unittests.scm && csi unittests.scm -e
	-rm unittests.scm test.mdal

.PHONY: clean
clean:
	-rm *.so *.import.scm bintracker bt.db
	-rm -rf docs plugins resources mame-bridge config roms
