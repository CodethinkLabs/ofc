FRONTEND = unfortranizer

BASE = src/

SRC_DIR = . prep parse parse/stmt
SRC_DIR_BASE = $(addprefix $(BASE),$(SRC_DIR))
LDFLAGS = -lm
CFLAGS = -O3 -Wall -Wextra -MD -MP

SRC = $(foreach dir, $(SRC_DIR_BASE), $(wildcard $(dir)/*.c))
OBJ = $(patsubst %.c, %.o, $(SRC))
DEB = $(patsubst %.c, %.d, $(SRC))

PREFIX = $(DESTDIR)/usr/local
BINDIR = $(PREFIX)/bin


all : $(FRONTEND)

$(FRONTEND) : $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(FRONTEND) $(OBJ) $(DEB)

install: $(FRONTEND)
	install $(FRONTEND) $(BINDIR)

uninstall:
	rm -f $(addprefix $(BINDIR)/,$(FRONTEND))

scan:
	@clang $(CFLAGS) -Weverything -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

scan-cc:
	@$(CC) $(CFLAGS) -o tempfile $(SRC) $(LDFLAGS) > /dev/null
	@rm tempfile

cppcheck:
	@cppcheck --enable=all --force $(SRC) > /dev/null

check: cppcheck scan
	scan-build $(MAKE) scan-cc

-include $(DEB)

.PHONY : all clean install uninstall scan scan-cc cppcheck check
