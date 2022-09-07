CFLAGS=-Wall -Wextra -Werror -g -O2

ALL: boot
	./boot test.ml

boot: boot.c
	$(CC) $(CFLAGS) -oboot boot.c
