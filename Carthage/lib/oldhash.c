/* Hash.C : sample hash-table for string-valued keys.
** In this version, no records:  string is in hash table or not.
*/
/* Revisions:
** 1996-04-23 : CMSMcQ : draft, using old entmgr.c as model
*/

/* #define DEBUGGING 1
*/
#include "mycat.h"
#include <string.h>

#ifdef DEBUGGING
void PrintHashtable(void);
#else
#define PrintHashtable(A) {}
#endif

extern int fDebug;

/* define table record structure */

typedef struct myrec * PRec;
typedef struct myrec {
    PRec prNext;             /* next entry in chain for this bucket */
    char *pcKey;             /* key of record */
    int  iValue;             /* value */
} MyRec;

/* a null prNext pointer marks end of list. */

#define HASHSIZE 101

/* eventually, we want to allow dynamic allocation of hash tables.
For now we assume a static one called rgRecs
*/

static PRec rgRecs[HASHSIZE]; /* array of records */

/**************************************************************/
/* hash                                                       */
/**************************************************************/
unsigned hash(char *psz) {
    /* revised (131 not 31) from Gonnet/Baeza-Yates */
    /* copied from K&R p.144 */
    unsigned hashval;

    for (hashval = 0; *psz != '\0'; psz++)
         hashval = *psz + 131 * hashval;
    return hashval % HASHSIZE;
}

/**************************************************************/
/* PRLookup(pc)                                               */
/**************************************************************/
PRec PRLookup(char *pcName) {
    PRec pr;

    for (pr = rgRecs[hash(pcName)]; pr != NULL; pr = pr->prNext)
         if (strcmp(pcName,pr->pcKey) == 0)
              return pr;    /* found */
    return NULL;            /* not found */
}

/**************************************************************/
/* FInstallRec(pc,i)                                          */
/**************************************************************/
/* FInstallRec()
    returns 1 if OK,
    0 if error,
    -1 if entity is already declared / installed
*/

int FInstallRec(char *pc,
               int  i) {
    PRec pr;
    unsigned hashval;

#ifdef DEBUGGING
    fprintf(stderr,"FInstallRec called for %s\n",pc);
#endif
    if ((pr = PRLookup(pc)) != NULL) {      /* found */
         return -1;                         /* don't touch a thing */
    } /* else */                            /* not found */
    pr = (PRec) malloc(sizeof(*pr));
    if (pr == NULL)
         return 0;
    hashval = hash(pc);
    pr->prNext = rgRecs[hashval];
    rgRecs[hashval] = pr;

    pr->pcKey = mycopy1(pc);
    pr->iValue = i;
#ifdef DEBUGGING
    fprintf(stderr,"FInstallRec installed %s\n",pc);
    PrintHashtable();
#endif
    return 1;
}


#ifdef DEBUGGING
void PrintHashtable(void) {
    PRec p;
    int i = 0;

    if ( ! fDebug) return;

    fprintf(stderr,"Printing hash table ...\n");
    for (p = rgRecs[0]; i < HASHSIZE ; p = rgRecs[i++]) {
         if (p != NULL) {
              for ( ; p != NULL ; p = p->prNext) {
                   fprintf(stderr,"%d.  %s (%d)\n"
                        i,p->pcKey, p->iValue);
              }
         }
    }
}
#endif

