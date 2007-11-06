# Make file for SGML DTD parser (dpp)
default: carthago

CC = gcc
CFLAGS = -Ilib
LIBFLAGS = -Llib -lmisc -lfl -lc
# LIBFLAGS = -lc -lfl -lmisc -L/usr/local/lib -L/homes/home1/cmsmcq/lib 

carthago: carthago.o mycat.o dpplex.o lllit.o lib/libmisc.a
	$(CC) $(CFLAGS) -o carthago carthago.o dpplex.o mycat.o lllit.o $(LIBFLAGS)

lib/libmisc.a:
	(cd lib; make)

carthago.o: carthago.c mycat.h dpplex.h lllit.h entmgr.h

carthago.c: carthago.y
	bison -d -k carthago.y -o carthago.c

mycat.o: mycat.c mycat.h

lllit.o: lllit.c lllit.h

msg.o: msg.c dppflags.h

entmgr.o: entmgr.c entmgr.h myfiles.c

dpplex.c:  dpplex.l dpp.tab.h
	flex -I -i -8 -odpplex.c dpplex.l

dpp.c: dpp.y
	bison dpp.y -o dpp.c

dpplex.o:  dpplex.c entmgr.c sgmldtd.h mycat.h dpp.tab.h myfiles.c

test:
	(cd tests; PATH=..:$$PATH ../carthage dirty.dtd clean.dtd ;	\
	diff clean.dtd myclean.dtd)


dpp: dpp.o mycat.o dpplex.o lllit.o 
	$(CC) $(CFLAGS) -odpp dpp.o dpplex.o mycat.o lllit.o $(LIBFLAGS)

dpp.o: dpp.c mycat.h dpplex.h lllit.h entmgr.h

clean:
	-rm *.o carthago
	-(cd lib; make clean)

realclean:	clean
	-rm carthago.c dpplex.c

distrib:
	(rm carthage.zip; cd ..; zip -r carthage carthage carthage; mv carthage.zip carthage)
