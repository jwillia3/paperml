.test: paperml
	./paperml paperml.al test.al
# 	./paperml test.al
	echo "done."

paperml: paperml.c
	$(CC) `pkg-config --cflags bdw-gc` \
		-Wall -Wextra -pedantic -std=c11 -g\
		-Dmalloc=GC_malloc \
		-o paperml \
		paperml.c \
		`pkg-config --libs bdw-gc`
