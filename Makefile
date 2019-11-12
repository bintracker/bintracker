CSC = csc
DOCGEN = scm2wiki
DOCS = bintracker-core.md bt-gui.md bt-state.md bt-types.md
LIBFLAGS = -s -d3
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0
ifdef CHICKEN_REPOSITORY_PATH
 CHICKEN_REPO_PATH = $(CHICKEN_REPOSITORY_PATH)
else
 CHICKEN_REPO_PATH = $(shell if [ ! -f "chicken-repository-path" ]; then\
 find /usr /home/ -type d 2>/dev/null | grep -P "lib.*?\/chicken\/9" >chicken-repository-path; fi;\
 head -n 1 chicken-repository-path)
endif
ALL_SOURCE_FILES = bt-types.scm bt-state.scm bt-gui.scm bintracker-core.scm\
 libmdal/schemta.scm libmdal/md-parser.scm libmdal/md-config.scm\
 libmdal/md-command.scm libmdal/utils/md-note-table.scm libmdal/md-types.scm\
 libmdal/md-helpers.scm libmdal/mdal.scm
MAKE_ETAGS = yes
ifeq ($(MAKE_ETAGS),yes)
 DO_TAGS = TAGS
endif

# build bintracker-core
bintracker-core.so: bintracker-core.scm bt-state.import.so bt-types.import.so\
	bt-gui.import.so libmdal/mdal.import.so $(DO_TAGS)
	export CHICKEN_REPOSITORY_PATH=$(CHICKEN_REPO_PATH):${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bintracker-core.scm -j bintracker-core
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm

bt-types.so: bt-types.scm
	$(CSC) $(LIBFLAGS) bt-types.scm -j bt-types

bt-types.import.so: bt-types.so
	$(CSC) $(IMPORTFLAGS) bt-types.import.scm

bt-state.so: bt-state.scm bt-types.import.so libmdal/mdal.import.so
	export CHICKEN_REPOSITORY_PATH=$(CHICKEN_REPO_PATH):${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bt-state.scm -j bt-state

bt-state.import.so: bt-state.so
	$(CSC) $(IMPORTFLAGS) bt-state.import.scm

bt-gui.so: bt-gui.scm bt-state.import.so bt-types.import.so
	export CHICKEN_REPOSITORY_PATH=$(CHICKEN_REPO_PATH):${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bt-gui.scm -j bt-gui

bt-gui.import.so: bt-gui.so
	$(CSC) $(IMPORTFLAGS) bt-gui.import.scm

TAGS: $(ALL_SOURCE_FILES)
	etags -r '"  (def.*? "' $(ALL_SOURCE_FILES)

%.md: %.scm
	$(DOCGEN) -i $< -o docs/generated/$@ -m

bintracker-core.md: bintracker-core.scm
bt-gui.md: bt-gui.scm
bt-state.md: bt-state.scm
bt-types.md: bt-types.scm

docs: $(DOCS)
	mkdocs build
	$(MAKE) docs -C libmdal

libmdal/mdal.import.so:
	$(MAKE) -C libmdal

.PHONY: clean
clean:
	-rm *.so *.import.scm
	$(MAKE) -C libmdal clean
