FRONTEND = unfortranizer

BASE = src/

SRC_DIR = . prep parse reformat parse/stmt
SRC_DIR_BASE = $(addprefix $(BASE),$(SRC_DIR))
LDFLAGS = -lm
CFLAGS = -O3 -Wall -Wextra -MD -MP

SRC = $(foreach dir, $(SRC_DIR_BASE), $(wildcard $(dir)/*.c))
OBJ = $(patsubst %.c, %.o, $(SRC))
DEB = $(patsubst %.c, %.d, $(SRC))

PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin

TEST_DIR = tests
TARGETS := $(wildcard $(TEST_DIR)/*.f $(TEST_DIR)/*.f77 $(TEST_DIR)/*.f90 $(TEST_DIR)/*.FOR)

all : $(FRONTEND)

$(FRONTEND) : $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(FRONTEND) $(OBJ) $(DEB)

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

-include $(DEB)

.PHONY : all clean install uninstall cppcheck scan scan-cc scan-build check tests $(TARGETS)
