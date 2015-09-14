FRONTEND = unfortranizer

BASE = src/

SRC_DIR = . parse parse/stmt
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

clean :
	rm -f $(FRONTEND) $(OBJ) $(DEB)

install: $(FRONTEND)
	install $(FRONTEND) $(BINDIR)

uninstall:
	rm -f $(addprefix $(BINDIR)/,$(FRONTEND))

-include $(DEB)

.PHONY : all clean install uninstall
