CFLAGS := -std=gnu11 -Wall -Wextra -Werror -Wfatal-errors -Wno-unused-function -Wno-trigraphs -fno-common -fomit-frame-pointer -fstrict-aliasing

all: bget.so

bget.so: bget.c
	gcc $(CFLAGS) "$<" -shared -o "$@"
