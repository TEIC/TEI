/* myfiles.c:  file-handling routines. */
/* Revisions: 
** 1995-02-17 : CMSMcQ : made file
*/
#include <stdio.h>
#include <string.h>
FILE *PFileOpenFnEnvMode(char *pcFn, char *pcEnvvar, char *pcMode) {

#define MAXPATHLEN 300
#define MAXFQFILEN 2 * MAXPATHLEN

    FILE *pf;
    char *pcEnvval;
    char pcPath[MAXPATHLEN];
    char *pcPrefix;
    char pcFqfi[MAXFQFILEN];

    /* fprintf(stderr,"entering PFileOpenFnEnvMode(%s,%s,%s)\n", 
	    pcFn,pcEnvvar,pcMode);
	    */
    if ((pcEnvval = getenv(pcEnvvar)) == 0) {
      /* fprintf(stderr,"Path not set\n"); */
      /* assert:  path has not been set */
      return fopen(pcFn,pcMode);
    } 
    /* assert:  path has been set */
    /* fprintf(stderr,"Path set to %s\n",pcEnvval); */
    strncpy(pcPath,pcEnvval,MAXPATHLEN);
    
    /* set up first fully-qualified file id */
    pcFqfi[0] = '\0';
    if ((pcPrefix = strtok(pcPath,":")) != 0) {
      strncpy(pcFqfi,pcPrefix,MAXFQFILEN);
      strcat(pcFqfi,"/");
      strncat(pcFqfi,pcFn,MAXFQFILEN - strlen(pcFqfi) -1);
      /* fprintf(stderr,"Fully qualified file id is %s\n",pcFqfi); */
    } else {
      fprintf(stderr,"PFOpenFnEntMode:  error opening file %s with path %s",
	      pcFn,pcPath);
      return NULL;
    }

    /* assert:  prefix has been identified */
    /* for each path prefix, try to open */
    while ((pf = fopen(pcFqfi,pcMode)) == NULL) {
      /* fprintf(stderr,"File %s could not be opened\n",pcFqfi); */
      pcPrefix = strtok(NULL,":");
      if (pcPrefix == NULL) return pf;
      strncpy(pcFqfi,pcPrefix,MAXFQFILEN);
      strcat(pcFqfi,"/");
      strncat(pcFqfi,pcFn,MAXFQFILEN - strlen(pcFqfi) -1);
      /* fprintf(stderr,"Fully qualified file id is %s\n",pcFqfi); */
    }
    return pf;
  }
