CSC = csc
DOCGEN = scm2wiki
DOCS = bintracker-core.md
LIBFLAGS = -s -d1
ifdef RELEASE
 LIBFLAGS += -O3
endif
IMPORTFLAGS = -s -d0

# build bintracker-core
bintracker-core: bintracker-core.scm
	export CHICKEN_REPOSITORY_PATH=/home/heinz/chickens/5.0.0/lib/chicken/9:${PWD}/libmdal;\
	$(CSC) $(LIBFLAGS) bintracker-core.scm -j bintracker-core
	$(CSC) $(IMPORTFLAGS) bintracker-core.import.scm
