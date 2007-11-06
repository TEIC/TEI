/* mycat.c:  miscellaneous string functions, esp concatenation */
/* Revisions:
** 14 Aug 94 : CMSMcQ : add PCUnquotePC(pc):  strip quotes
**                      (actually, all it does is strip first and last)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
int iMac, iCur, iCopy;
/* mycat.c:  string copying and concatenation routines */
/* mycopy1:  allocate space for one string, copy argument there,
return pointer */
char *mycopy1(char *pc) {
    char *sp;
    sp = (char *)malloc(strlen(pc) + 1);
    if (sp != NULL) {
         strcpy(sp,pc);
         return sp;
    } else {
         fprintf(stderr, "Mycopy1:  failed getting memory for %s\n",
                   pc);
         exit(16);
    }
}
/* mycat2, 3, 4:  concatenate 2, 3, or 4 string arguments,
returning pointer to the new string */
char *mycat2(char *pc1, char *pc2) {
    char *sp;
    sp = (char *)malloc(strlen(pc1) + strlen(pc2) + 1);
    if (sp != NULL) {
         strcpy(sp,pc1);
         sp = strcat(sp,pc2);
         return sp;
    } else {
         fprintf(stderr, "Mycat2:  failed getting memory for %s%s\n",
                   pc1,pc2);
         exit(16);
    }
}
char *mycat3(char *pc1, char *pc2, char *pc3) {
    char *sp;
    sp = (char *)malloc(strlen(pc1) + strlen(pc2) +
         strlen(pc3) + 1);
    if (sp != NULL) {
         strcpy(sp,pc1);
         sp = strcat(strcat(sp,pc2),pc3);
         return sp;
    } else {
         fprintf(stderr,
              "Mycat3:  failed getting memory for %s%s%s\n",pc1,pc2,pc3);
         exit(16);
    }
}
char *mycat4(char *pc1, char *pc2, char *pc3, char *pc4) {
    char *sp;
    sp = (char *)malloc(strlen(pc1) + strlen(pc2) +
         strlen(pc3) + strlen(pc4) + 1);
    if (sp != NULL) {
         strcpy(sp,pc1);
         sp = strcat(strcat(strcat(sp,pc2),pc3),pc4);
         return sp;
    } else {
         fprintf(stderr,
                   "Mycat4:  failed getting memory for %s%s%s%s\n",
                   pc1,pc2,pc3,pc4);
         exit(16);
    }
}
char *mycat5(char *pc1, char *pc2, char *pc3, char *pc4, char *pc5) {
    char *sp;
    sp = (char *)malloc(strlen(pc1) + strlen(pc2) +
         strlen(pc3) + strlen(pc4) + strlen(pc5) + 1);
    if (sp != NULL) {
         strcpy(sp,pc1);
         sp = strcat(strcat(strcat(strcat(sp,pc2),pc3),pc4),pc5);
         return sp;
    } else {
         fprintf(stderr,
                   "Mycat5:  failed getting memory for %s%s%s%s%s\n",
                   pc1,pc2,pc3,pc4,pc5);
         exit(16);
    }
}

/* myescape(x):  save a string as in lsave, but insert backslash
escapes for newline and backslash. */
char *myescape(char *pSrc) {
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

char *PCUnquotePC(char *pIn) {
    char *pOut;
    char *pCur;
    int  iCur, iLen;

    if (pIn == NULL) return NULL;
    pOut = (char *)malloc(strlen(pIn));
    if (pOut != NULL) {
         /* skip first character, continue through to last */
         for (pCur = pOut; pIn[1]; *pCur++ = *++pIn)
              ;
         *--pCur = '\0';
         return pOut;
    } else {
         fprintf(stderr, "PCUnquotePC:  failed getting memory for %s\n",
                   pIn);
         exit(16);
    }
}

char * PCCleanperef(char * pcDirty) {
    char * pcT1;
    char * pcT2;
    char * pcClean;

    pcClean = mycopy1(pcDirty);

    for (pcT1 = pcT2 = pcClean; *pcT1 ; pcT1++) {
         if ((*pcT1 != '%')  && (*pcT1 != ';')
          && (*pcT1 != '\n') && (*pcT1 != '\r') && (*pcT1 != '\t')) {
              *pcT2++ = *pcT1;
         }
    }
    *pcT2 = '\0';
    return pcClean;
}
