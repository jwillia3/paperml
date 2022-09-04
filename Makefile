CFLAGS=-Wall -Wextra -Werror -Wno-array-bounds -Wno-infinite-recursion -g -O2

ALL: boot
	./boot test.ml

boot: boot.c
	$(CC) $(CFLAGS) -oboot boot.c
