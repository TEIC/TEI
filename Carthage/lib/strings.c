/* strings.c:  miscellaneous string functions, esp concatenation */
/* Revisions:
** 28 Aug 95 : CMSMcQ : make strings.c from mycat.c
** 14 Aug 94 : CMSMcQ : add PCUnquotePC(pc):  strip quotes
**                      (actually, all it does is strip first and last)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
int iMac, iCur, iCopy;
/* PsCopyPs:  allocate space for one string, copy argument there,
return pointer */
char *PsCopyPs(char *ps) {
    char *psNew;
    psNew = (char *)malloc(strlen(ps) + 1);
    if (psNew != NULL) {
         strcpy(psNew,ps);
         return psNew;
    } else {
         fprintf(stderr, "PsCopyPs:  failed getting memory for %s\n",
                   ps);
         exit(16);
    }
}
/* PsCatPs2, 3, 4, 5:  concatenate 2 to 5 string arguments,
returning pointer to the new string */
char *PsCatPs2(char *ps1, char *ps2) {
    char *psNew;
    psNew = (char *)malloc(strlen(ps1) + strlen(ps2) + 1);
    if (psNew != NULL) {
         strcpy(psNew,ps1);
         psNew = strcat(psNew,ps2);
         return psNew;
    } else {
         fprintf(stderr, "PsCatPs2:  failed getting memory for %s%s\n",
                   ps1,ps2);
         exit(16);
    }
}
char *PsCatPs3(char *ps1, char *ps2, char *ps3) {
    char *psNew;
    psNew = (char *)malloc(strlen(ps1) + strlen(ps2) +
         strlen(ps3) + 1);
    if (psNew != NULL) {
         strcpy(psNew,ps1);
         psNew = strcat(strcat(psNew,ps2),ps3);
         return psNew;
    } else {
         fprintf(stderr,
              "PsCatPs3:  failed getting memory for %s%s%s\n",ps1,ps2,ps3);
         exit(16);
    }
}
char *PsCatPs4(char *ps1, char *ps2, char *ps3, char *ps4) {
    char *psNew;
    psNew = (char *)malloc(strlen(ps1) + strlen(ps2) +
         strlen(ps3) + strlen(ps4) + 1);
    if (psNew != NULL) {
         strcpy(psNew,ps1);
         psNew = strcat(strcat(strcat(psNew,ps2),ps3),ps4);
         return psNew;
    } else {
         fprintf(stderr,
                   "PsCatPs4:  failed getting memory for %s%s%s%s\n",
                   ps1,ps2,ps3,ps4);
         exit(16);
    }
}
char *PsCatPs5(char *ps1, char *ps2, char *ps3, char *ps4, char *ps5) {
    char *psNew;
    psNew = (char *)malloc(strlen(ps1) + strlen(ps2) +
         strlen(ps3) + strlen(ps4) + strlen(ps5) + 1);
    if (psNew != NULL) {
         strcpy(psNew,ps1);
         psNew = strcat(strcat(strcat(strcat(psNew,ps2),ps3),ps4),ps5);
         return psNew;
    } else {
         fprintf(stderr,
                   "PsCatPsPs5:  failed getting memory for %s%s%s%s%s\n",
                   ps1,ps2,ps3,ps4,ps5);
         exit(16);
    }
}

/* PsEscapePs(x):  save a string as in lsave, but insert backslash
escapes for newline and backslash. */
char *PsEscapePs(char *pSrc) {
    char * pNew;
    pNew = (char*)malloc(2 * strlen(pSrc)+1);
    if (pNew != NULL) {
         iMac = strlen(pSrc);
         iCopy = 0;
         for (iCur=0;iCur<iMac;iCur++) {
              if (pSrc[iCur] == '\\') {
                   pNew[iCopy++] = '\\';
                   pNew[iCopy++] = '\\';
              } else if (pSrc[iCur] == '\n') {
                   pNew[iCopy++] = '\\';
                   pNew[iCopy++] = 'n';
              } else if (pSrc[iCur] == '\t') {
                   pNew[iCopy++] = '\\';
                   pNew[iCopy++] = 't';
              } else if (pSrc[iCur] == '\r') {
                        ;
                   /* suppress carriage return!  */
                   /* pNew[iCopy++] = '\\';      */
                   /* pNew[iCopy++] = 'r';       */
              } else {
                   pNew[iCopy++] = pSrc[iCur];
              }
         }
         pNew[iCopy] = '\0';
         return pNew;
    } else {
         fprintf(stderr,"Died converting %s ...",pSrc);
         exit(16);
    }
}

char *PsUnquotePs(char *pIn) {
    char *pOut;
    char *pCur;

    if (pIn == NULL) return NULL;
    pOut = (char *)malloc(strlen(pIn));
    if (pOut != NULL) {
         /* skip first character, continue through to last */
         for (pCur = pOut; pIn[1]; *pCur++ = *++pIn)
              ;
         *--pCur = '\0';
         return pOut;
    } else {
         fprintf(stderr, "PsUnquotePs:  failed getting memory for %s\n",
                   pIn);
         exit(16);
    }
}

