CFLAGS=-Wall -Wextra -Werror -g -O2

ALL: boot
	./boot ml.ml

boot: boot.c
	$(CC) $(CFLAGS) -oboot boot.c
