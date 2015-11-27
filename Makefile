FRONTEND = ofc
FRONTEND_DEBUG = $(FRONTEND)-debug

BASE = src/

SRC_DIR = . prep parse sema reformat parse/stmt sema/stmt
SRC_DIR_BASE = $(addprefix $(BASE),$(SRC_DIR))
LDFLAGS = -lm
CFLAGS_COMMON = -Wall -Wextra -Werror -MD -MP -I include
CFLAGS = -O3 $(CFLAGS_COMMON)
CFLAGS_DEBUG = -O0 -g $(CFLAGS_COMMON)

SRC = $(foreach dir, $(SRC_DIR_BASE), $(wildcard $(dir)/*.c))
OBJ = $(patsubst %.c, %.o, $(SRC))
OBJ_DEBUG = $(patsubst %.c, %.debug.o, $(SRC))
DEB = $(patsubst %.c, %.d, $(SRC))
DEB_DEBUG = $(patsubst %.c, %.debug.d, $(SRC))

PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin

TEST_DIR = tests
TARGETS := $(sort $(wildcard $(TEST_DIR)/*.f $(TEST_DIR)/*.f77 $(TEST_DIR)/*.f90 $(TEST_DIR)/*.FOR))

all : $(FRONTEND)

$(FRONTEND) : $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(FRONTEND_DEBUG): $(OBJ_DEBUG)
	$(CC) $(CFLAGS_DEBUG) -o $@ $^ $(LDFLAGS)

$(OBJ_DEBUG) : %.debug.o : %.c
	$(CC) $(CFLAGS_DEBUG) -c -o $@ $<

debug: $(FRONTEND_DEBUG)

clean:
	rm -f $(FRONTEND) $(FRONTEND_DEBUG) $(OBJ) $(OBJ_DEBUG) $(DEB) $(DEB_DEBUG)

install: $(FRONTEND)
	install $(FRONTEND) $(BINDIR)

uninstall:
	rm -f $(addprefix $(BINDIR)/,$(FRONTEND))

cppcheck:
	@cppcheck --enable=all --force $(SRC) > /dev/null

scan:
	@clang $(CFLAGS) -Weverything -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

scan-cc:
	@$(CC) $(CFLAGS) -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

scan-build:
	@scan-build $(MAKE) scan-cc

check: cppcheck scan scan-build

tests: $(TARGETS)

$(TARGETS): $(FRONTEND)
	@$(realpath $(FRONTEND)) $@ > /dev/null

loc:
	@wc -l $(SRC)

-include $(DEB)

.PHONY : all clean install uninstall debug cppcheck scan scan-cc scan-build check tests $(TARGETS) loc
