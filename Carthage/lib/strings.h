/* strings.h:  miscellaneous string functions, esp concatenation */
/* Revisions:
** 28 Aug 95 : CMSMcQ : make strings.h from strings.c
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
char *PsCopyPs(char *ps);
char *PsCatPs2(char *ps1, char *ps2);
char *PsCatPs3(char *ps1, char *ps2, char *ps3);
char *PsCatPs4(char *ps1, char *ps2, char *ps3, char *ps4);
char *PsCatPs5(char *ps1, char *ps2, char *ps3, char *ps4, char *ps5);
char *PsEscapePs(char *pSrc);
char *PsUnquotePs(char *pIn);
