CSC = csc
DOCGEN = scm2wiki
DOCS = bintracker-core.md
LIBFLAGS = -s -d3
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0

# build bintracker-core
bintracker-core: bintracker-core.scm bt-state.import.so bt-types.import.so bt-gui.import.so mdal
	export CHICKEN_REPOSITORY_PATH=/home/heinz/chickens/5.0.0/lib/chicken/9:${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bintracker-core.scm -j bintracker-core
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm

bt-types.so: bt-types.scm
	$(CSC) $(LIBFLAGS) bt-types.scm -j bt-types

bt-types.import.so: bt-types.so
	$(CSC) $(IMPORTFLAGS) bt-types.import.scm

bt-state.so: bt-state.scm bt-types.import.so mdal
	export CHICKEN_REPOSITORY_PATH=/home/heinz/chickens/5.0.0/lib/chicken/9:${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bt-state.scm -j bt-state

bt-state.import.so: bt-state.so
	$(CSC) $(IMPORTFLAGS) bt-state.import.scm

bt-gui.so: bt-gui.scm bt-state.import.so bt-types.import.so
	export CHICKEN_REPOSITORY_PATH=/home/heinz/chickens/5.0.0/lib/chicken/9:${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bt-gui.scm -j bt-gui

bt-gui.import.so: bt-gui.so
	$(CSC) $(IMPORTFLAGS) bt-gui.import.scm

%.md: %.scm
	$(DOCGEN) -i $< -o docs/generated/$@ -m

bintracker-core.md: bintracker-core.scm
bt-types.md: bt-types.scm
bt-gui.md: bt-gui.scm

docs: $(DOCS)
	mkdocs build

.PHONY: mdal
mdal:
	$(MAKE) -C libmdal

.PHONY: clean
clean:
	-rm *.so *.import.scm
	$(MAKE) -C libmdal clean
