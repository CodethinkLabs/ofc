FRONTEND = ofc
FRONTEND_DEBUG = $(FRONTEND)-debug

BASE = src/

SRC_DIR = . prep parse sema global parse/stmt sema/stmt sema/pass
SRC_DIR_BASE = $(addprefix $(BASE),$(SRC_DIR))

GCC_VER_MAJ = $(shell gcc -dumpversion | cut -f 1 -d '.')
GCC_VER_MIN = $(shell gcc -dumpversion | cut -f 2 -d '.')

GCC_VER_MAJ_SUP = 4
GCC_VER_MIN_SUP = 8

GCC_VER_SUPPORTED = $(shell [ $(GCC_VER_MAJ) -gt $(GCC_VER_MAJ_SUP) -o \( $(GCC_VER_MAJ) -eq $(GCC_VER_MAJ_SUP) -a $(GCC_VER_MIN) -ge $(GCC_VER_MIN_SUP) \) ] && echo true)

ifeq ($(GCC_VER_SUPPORTED),true)
	CFLAGS_WERROR = -Werror
else
	CFLAGS_WERROR = $(warning Your GCC version is too old to be supported, please upgrade to $(GCC_VER_MAJ_SUP).$(GCC_VER_MIN_SUP) or above)
endif

LDFLAGS = -lm
CFLAGS_COMMON = -Wall -Wextra $(CFLAGS_WERROR) -std=gnu99 -MD -MP -I include
CFLAGS += -O3 $(CFLAGS_COMMON)
CFLAGS_DEBUG += -O0 -g $(CFLAGS_COMMON)

export OFC_GIT_COMMIT = $(shell git rev-parse HEAD)
export OFC_GIT_BRANCH = $(shell git rev-parse --symbolic-full-name --abbrev-ref HEAD)

SRC = $(foreach dir, $(SRC_DIR_BASE), $(wildcard $(dir)/*.c))
OBJ = $(patsubst %.c, %.o, $(SRC))
OBJ_DEBUG = $(patsubst %.c, %.debug.o, $(SRC))
DEB = $(patsubst %.c, %.d, $(SRC))
DEB_DEBUG = $(patsubst %.c, %.debug.d, $(SRC))

TEST_DIR = tests

PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin

all : $(FRONTEND)

$(FRONTEND) : $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(FRONTEND_DEBUG): $(OBJ_DEBUG)
	$(CC) $(CFLAGS_DEBUG) -o $@ $^ $(LDFLAGS)

$(OBJ_DEBUG) : %.debug.o : %.c
	$(CC) $(CFLAGS_DEBUG) -c -o $@ $<

debug: $(FRONTEND_DEBUG)

clean:
	rm -f $(FRONTEND) $(FRONTEND_DEBUG) $(OBJ) $(OBJ_DEBUG) \
	$(DEB) $(DEB_DEBUG)

install: $(FRONTEND)
	install -d $(BINDIR)
	install $(FRONTEND) $(BINDIR)

uninstall:
	rm -f $(addprefix $(BINDIR)/,$(FRONTEND))

cppcheck:
	@cppcheck --enable=all --force $(SRC) > /dev/null

scan:
	@clang $(CFLAGS) -Weverything -Wno-reserved-id-macro -Wno-padded -Wno-zero-length-array -Wno-vla -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

scan-cc:
	@$(CC) $(CFLAGS) -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

scan-build:
	@scan-build $(MAKE) scan-cc

check: cppcheck scan scan-build

test: $(FRONTEND) $(FRONTEND_DEBUG)
	$(MAKE) FRONTEND=$(realpath $(FRONTEND)) $(realpath FRONTEND_DEBUG=$(FRONTEND_DEBUG)) -C $(TEST_DIR) test

test-report: $(FRONTEND) $(FRONTEND_DEBUG)
	$(MAKE) FRONTEND=$(realpath $(FRONTEND)) $(realpath FRONTEND_DEBUG=$(FRONTEND_DEBUG)) -C $(TEST_DIR) test-report

test-report-lite: $(FRONTEND)
	$(MAKE) FRONTEND=$(realpath $(FRONTEND)) $(realpath FRONTEND_DEBUG=$(FRONTEND_DEBUG)) -C $(TEST_DIR) test-report-lite

loc:
	@wc -l $(SRC)

-include $(DEB) $(DEB_DEBUG)

.PHONY : all clean install uninstall debug cppcheck scan scan-cc scan-build check test test-report test-report-lite loc
