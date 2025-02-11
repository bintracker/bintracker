CSC = csc
DOCGEN = scm2wiki
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
VENV = .venv
VENV_PYTHON = $(VENV)/bin/python3
SYSTEM_PYTHON = $(or $(shell which python3), $(shell which python))
PYTHON = $(or $(wildcard $(VENV_PYTHON)), $(SYSTEM_PYTHON))

# Might need to use csc -compile-syntax in places. See:
# https://lists.nongnu.org/archive/html/chicken-users/2017-08/msg00004.html

.PHONY: bintracker
bintracker: mame-bridge config roms plugins resources aux-files mdal-targets
	csm -program bintracker -ignore ../plugins/ ..

schemta.so: $(MDAL_SRC)schemta.scm
	$(CSC) $(LIBFLAGS) $< -j schemta -emit-types-file schemta.types -o $@

schemta.import.so: schemta.so
	$(CSC) $(IMPORTFLAGS) schemta.import.scm

# Schemta standalone executable
schemta-exe: $(MDAL_SRC)schemta-exe.scm schemta.import.so
	$(CSC) $< -b -O3 -compile-syntax -o $@

$(VENV_PYTHON):
	rm -rf $(VENV)
	$(SYSTEM_PYTHON) -m venv $(VENV)

venv: $(VENV_PYTHON)

doc-deps:
	$(PYTHON) -m pip install --upgrade pip
	$(PYTHON) -m pip install -r ../requirements.txt

.PHONY: venv doc-deps

doc-toolchain: venv doc-deps

# generate documentation
gendoc/generated/%.md: $(BT_SRC)%.scm
	$(DOCGEN) -a -i $< -o $@ -m

gendoc/generated/%.md: $(MDAL_SRC)%.scm
	$(DOCGEN) -a -i $< -o $@ -m

gendoc/generated/bintracker-core.md: $(BT_SRC)bintracker-core.scm
gendoc/generated/bt-gui.md: $(BT_SRC)bt-gui.scm
gendoc/generated/bt-gui-lolevel.md: $(BT_SRC)bt-gui-lolevel.scm
gendoc/generated/bt-maths.md: $(BT_SRC)bt-maths.scm
gendoc/generated/bt-state.md: $(BT_SRC)bt-state.scm
gendoc/generated/bt-types.md: $(BT_SRC)bt-types.scm
gendoc/generated/bt-emulation.md: $(BT_SRC)bt-emulation.scm
gendoc/generated/bt-db.md: $(BT_SRC)bt-db.scm
gendoc/generated/mdal.md: $(MDAL_SRC)mdal.scm
gendoc/generated/md-def.md: $(MDAL_SRC)md-def.scm
gendoc/generated/md-helpers.md: $(MDAL_SRC)md-helpers.scm
gendoc/generated/md-types.md: $(MDAL_SRC)md-types.scm
gendoc/generated/md-parser.md: $(MDAL_SRC)md-parser.scm
gendoc/generated/md-command.md: $(MDAL_SRC)md-command.scm
gendoc/generated/utils/md-audio.md: $(MDAL_SRC)utils/md-audio.scm
gendoc/generated/utils/md-note-table.md: $(MDAL_SRC)utils/md-note-table.scm
gendoc/generated/schemta.md: $(MDAL_SRC)schemta.scm

docs: gendoc-dirs\
 plugin-docs\
 gendoc/generated/bintracker-core.md\
 gendoc/generated/bt-gui.md\
 gendoc/generated/bt-gui-lolevel.md\
 gendoc/generated/bt-maths.md\
 gendoc/generated/bt-state.md\
 gendoc/generated/bt-types.md\
 gendoc/generated/bt-db.md\
 gendoc/generated/bt-emulation.md\
 gendoc/generated/mdal.md\
 gendoc/generated/md-def.md\
 gendoc/generated/md-helpers.md\
 gendoc/generated/md-types.md\
 gendoc/generated/md-parser.md\
 gendoc/generated/md-command.md\
 gendoc/generated/schemta.md\
 gendoc/generated/utils/md-audio.md\
 gendoc/generated/utils/md-note-table.md
	for file in ../docs/*.md; do cp $$file gendoc/; done
	for file in ../libmdal/docs/*.md; do cp $$file gendoc/; done
	cp -r ../docs/assets/ gendoc/
	cp -r ../docs/images/ gendoc/
	cd .. && ./build/$(PYTHON) -m mkdocs build

gendoc-dirs:
	mkdir -p gendoc/
	mkdir -p gendoc/plugins/
	mkdir -p gendoc/generated/
	mkdir -p gendoc/generated/utils/

# Generate ETags file for Emacs
TAGS: $(BT_SRC)bt-types.scm\
 $(BT_SRC)bt-maths.scm\
 $(BT_SRC)bt-state.scm\
 $(BT_SRC)bt-gui-lolevel.scm\
 $(BT_SRC)bt-gui.scm\
 $(BT_SRC)bt-db.scm\
 $(BT_SRC)bt-emulation.scm\
 $(BT_SRC)bintracker-core.scm\
 $(MDAL_SRC)schemta.scm\
 $(MDAL_SRC)md-parser.scm\
 $(MDAL_SRC)md-def.scm\
 $(MDAL_SRC)md-command.scm\
 $(MDAL_SRC)utils/md-audio.scm\
 $(MDAL_SRC)utils/md-note-table.scm\
 $(MDAL_SRC)md-types.scm\
 $(MDAL_SRC)md-helpers.scm\
 $(MDAL_SRC)mdal.scm
	cd .. && etags -r '"  (def.*? "' bt-maths.scm bt-types.scm bt-state.scm\
 bt-gui-lolevel.scm bt-gui.scm bt-db.scm bt-emulation.scm bintracker-core.scm\
 libmdal/schemta.scm libmdal/md-parser.scm libmdal/md-def.scm\
 libmdal/md-command.scm libmdal/md-types.scm libmdal/md-helpers.scm\
 libmdal/utils/md-audio.scm libmdal/utils/md-note-table.scm libmdal/mdal.scm

.PHONY: mame-bridge
mame-bridge:
	cp -r $(BT_SRC)mame-bridge ./

.PHONY: config
config:
	cp -r $(BT_SRC)config/ .

.PHONY: roms
roms:
	cp -r $(BT_SRC)roms/ .
ifneq (,$(wildcard roms/spectrum.rom))
	mv roms/spectrum/opense.rom roms/spectrum/spectrum.rom
endif

.PHONY: plugins
plugins:
	cp -r $(BT_SRC)plugins .

.PHONY: plugin-docs
plugin-docs:
	mkdir -p gendoc/plugins/mml
	mkdir -p gendoc/plugins/pcmconv
	mkdir -p gendoc/plugins/prng
	mkdir -p gendoc/plugins/snippets
	mkdir -p gendoc/plugins/unzufall
	cp $(BT_SRC)plugins/mml/README.md gendoc/plugins/mml/
	cp $(BT_SRC)plugins/pcmconv/README.md gendoc/plugins/pcmconv/
	cp $(BT_SRC)plugins/prng/README.md gendoc/plugins/prng/
	cp $(BT_SRC)plugins/snippets/README.md gendoc/plugins/snippets/
	cp $(BT_SRC)plugins/unzufall/README.md gendoc/plugins/unzufall/

.PHONY: resources
resources:
	mkdir -p resources/
	cp -r $(BT_SRC)resources/icons resources/

.PHONY: aux-files
aux-files:
	cp -r ../mdef .
	cp -r ../tunes .

.PHONY: mdal-targets
mdal-targets:
	cp -r $(MDAL_SRC)mdal-targets .

# run unit tests
.PHONY: tests
tests: schemta-tests

.PHONY: schemta-tests
schemta-tests: m6502-tests m6809-tests i8080-tests

.PHONY: m6809-tests
m6809-tests: schemta-exe mdal-targets
	cp -t ./ $(MDAL_SRC)schemta-tests/m6809.tests.scm && csi m6809.tests.scm -e
	-rm m6809.tests.scm

.PHONY: m6502-tests
m6502-tests: schemta-exe mdal-targets
	cp -t ./ $(MDAL_SRC)schemta-tests/m6502.tests.scm && csi m6502.tests.scm -e
	-rm m6502.tests.scm

.PHONY: i8080-tests
i8080-tests: schemta-exe mdal-targets
	cp -t ./ $(MDAL_SRC)schemta-tests/i8080.tests.scm && csi i8080.tests.scm -e
	-rm i8080.tests.scm

.PHONY: clean
clean:
	-rm *.so *.o *.import.scm bintracker schemta-exe bt.db
	-rm -rf docs plugins resources mame-bridge config roms mdef tunes mdal-targets gendoc documentation
