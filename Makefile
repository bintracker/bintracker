CSC = csc
DOCGEN = scm2wiki
DOCS = bintracker-core.md
LIBFLAGS = -s -d1
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0

# build bintracker-core
bintracker-core: bintracker-core.scm bt-types.import.so mdal
	export CHICKEN_REPOSITORY_PATH=/home/heinz/chickens/5.0.0/lib/chicken/9:${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bintracker-core.scm -j bintracker-core
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm

bt-types.so: bt-types.scm
	$(CSC) $(LIBFLAGS) bt-types.scm -j bt-types

bt-types.import.so: bt-types.so
	$(CSC) $(IMPORTFLAGS) bt-types.import.scm

.PHONY: mdal
mdal:
	$(MAKE) -C libmdal

.PHONY: clean
clean:
	-rm *.so *.import.scm
	$(MAKE) -C libmdal clean
