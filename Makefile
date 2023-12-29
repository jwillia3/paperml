.test: paperml
	./paperml test.al

paperml: paperml.c
	$(CC) -Wall -Wextra -pedantic -std=c11 -g -o paperml paperml.c
