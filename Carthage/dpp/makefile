# Make file for SGML lexical analyzer

CC = gcc
CFLAGS = -I/usr/local/include
LIBFLAGS = -lc -lfl -L/usr/local/lib 

dpp: dpp.tab.o mycat.o lex.yy.o lllit.o msg.o
	$(CC) $(CFLAGS) -odpp dpp.tab.o lex.yy.o mycat.o lllit.o msg.o $(LIBFLAGS)

dpp.tab.o: dpp.tab.c mycat.h dpplex.h lllit.h

dpp.tab.c: dpp.y
	bison dpp.y

mycat.o: mycat.c mycat.h

lllit.o: lllit.c lllit.h

msg.o: msg.c dppflags.h

entmgr.o: entmgr.c entmgr.h myfiles.c

lex.yy.c:  dpplex.l dpp.tab.h
	flex -I -i dpplex.l

lex.yy.o:  lex.yy.c entmgr.c sgmldtd.h mycat.h dpp.tab.h

.y.c :
	bison $*.y

