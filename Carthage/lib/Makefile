# make file for private library

CC = gcc
CFLAGS =
LIBFLAGS = -lc -lfl 

lib : msg.o strings.o hash.o
	ar rc libmisc.a *.o

clean:
	-rm *.o *.a