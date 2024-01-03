.test: paperml
	./paperml paperml.al test.al

paperml: paperml.c
	$(CC) `pkg-config --cflags bdw-gc` \
		-Wall -Wextra -pedantic -std=c11 -g\
		-o paperml \
		paperml.c \
		`pkg-config --libs bdw-gc`
