/* filesrch.c:  search for a file in the directories named by
DPP_PATH. */
#include <stdio.h>
#include "myfiles.c"

int main(int argc, char *argv[]) {
  FILE *pf;
  int c;
  int i;

  for (i = 1; i < argc; i++) {
    fprintf(stderr,"\nBegin search for file %s\n",argv[i]);
    pf = PFileOpenFnEnvMode(argv[i],"DPP_PATH","r");
    if (pf == NULL) {
      fprintf(stderr,"File %s was not found.\n",argv[i]);
    } else {
      while ((c = fgetc(pf)) != EOF)
	putchar(c);
      fprintf(stdout,"[eof]\n");
    }
  }
  exit(0);
}
