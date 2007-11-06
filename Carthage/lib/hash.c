/* Hash.C : sample hash-table for string-valued keys.
** In this version, no records:  string is in hash table or not.
*/
/* Revisions:
** 1996-04-24 : CMSMcQ : revise into real library routines
** 1996-04-23 : CMSMcQ : draft, using old entmgr.c as model
*/

/*
This library defines a hashtable datatype (HASHTABLE) 
and implements the following routines for managing such tables:

  HtInit(void) - initializes a hash table and returns a handle
  FDropHt(ht)  - frees an existing hash table
  
  FInstallKeyHtPRec(char *,ht,void *) - installs a record with the
                 given key and payload in the named hash table
  PPRecFindKeyHt(char *,ht) - finds a record in the named hash table
                 and returns a pointer to its pRec record; 
                 the value returned is null if no such record exists;
                 the value pointed at is null if there is no payload.
  FDropKeyHt(char *,ht) - removes a record from a hash table

  ICardinalityHt(ht) - returns number of items in the hash table
  HtDiffHtHt(ht,ht) - returns a hash table representing the set
                 difference between two hash tables
  HtInterHtHt(ht,ht) - returns a hash table representing the set
                 intersection of two hash tables
  HtUnionHtHt(ht,ht) - returns a hash table representing the set
                 union of two hash tables
  HtllFromHt(ht) - returns a linked list of hash-table records, containing
                 all the members of a hash table; the format is
                 that of a one-bucket hash table

  InspectHt(ht) - writes to stderr a report on what's in the hash
                 table, number of buckets, etc.
  PrintHashTable(ht) -

(In version 1, all I implement are HtInit(), FInstallKeyHtPRec(), 
and PPRecFindKeyHt().  The others will have to wait; I'm in a hurry
to use this, and I don't need removal, destruction, or administrative
routines for my immediate purposes.
*/

/* #define DEBUGGING 1
*/

#include "strings.h"
#include <string.h>

#ifdef DEBUGGING
void PrintHashtable(void);
#else
#define PrintHashtable(A) {}
#endif

extern int fDebug;

/* define table record structure */

typedef struct htrec * phtrec;
typedef struct htrec {
    char * pcKey;             /* key of this record */
    phtrec prNext;            /* next entry in chain for this bucket */
    void * pRecord;           /* payload record */
} HashTableRec;

/* a null prNext pointer marks end of list. */

#define HASHSIZE 1009

/* eventually, we want the usero allow allocation of hash tables
For now we assume a static one called rgRecs
*/

typedef phtrec * HASHTABLE;
typedef phtrec * HTLINKEDLIST;

/**************************************************************/
/* HtInit()                                                   */
/**************************************************************/
HASHTABLE HtInit(void) {
  return (HASHTABLE) calloc(HASHSIZE,sizeof(phtrec));
};

/**************************************************************/
/* FDropHt(ht)                                                */
/**************************************************************/
void DropHt(HASHTABLE ht) {
  free(ht);
};

/**************************************************************/
/* IHashPc                                                    */
/**************************************************************/
unsigned IHashPc(char *psz) {
    /* revised (131 not 31) from Gonnet/Baeza-Yates */
    /* copied from K&R p.144 */
    unsigned hashval;

    for (hashval = 0; *psz != '\0'; psz++)
         hashval = *psz + 131 * hashval;
    return hashval % HASHSIZE;
}

/**************************************************************/
/* PPRLookupKeyHt(pc)                                         */
/**************************************************************/
void * PPRLookupKeyHt(char *pc, HASHTABLE ht) {
    phtrec pr;

    for (pr = ht[IHashPc(pc)]; pr != NULL; pr = pr->prNext)
         if (strcmp(pc,pr->pcKey) == 0)
              return &(pr->pRecord); /* found */
    return NULL;                     /* not found */
}

/**************************************************************/
/* FInstallKeyHtPRec(pc,ht,prec)                              */
/**************************************************************/
/* FInstallKeyHtPRec()
    returns 1 if OK,
    0 if error,
    -1 if entity is already declared / installed
*/

int FInstallKeyHtPRec(char      *pc,
                      HASHTABLE ht,
                      void *    pRec) {
    phtrec pr;
    unsigned hashval;

#ifdef DEBUGGING
    fprintf(stderr,"FInstallRec called for %s\n",pc);
#endif
    if ((pr = PPRLookupKeyHt(pc,ht)) != NULL) { /* found */
         return -1;                             /* don't touch a thing */
    } /* else */                                /* not found */
    pr = (phtrec) malloc(sizeof(*pr));
    if (pr == NULL)
         return 0;
    hashval = IHashPc(pc);
    pr->prNext = ht[hashval];
    ht[hashval] = pr;

    pr->pcKey = PsCopyPs(pc);
    pr->pRecord = pRec;
#ifdef DEBUGGING
    fprintf(stderr,"FInstallRec installed %s\n",pc);
    PrintHashtable();
#endif
    return 1;
};

 int ICardinalityHt(HASHTABLE ht) {
   /* returns number of items in the hash table */
   int c = 0;
   int i = 0;
   phtrec p;

   for (p = ht[0]; i < HASHSIZE ; p = ht[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 c++;
       }
     }
   }
   return c;
 };
 
 HASHTABLE HtDiffHtHt(HASHTABLE ht1,HASHTABLE ht2) {
   /* returns a hash table representing the set 
      difference between two hash tables */
   int i = 0;
   phtrec p;
   HASHTABLE htDiff = HtInit();

   for (p = ht1[i]; i < HASHSIZE ; p = ht1[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* test and possibly insert into result */
	 if (PPRLookupKeyHt(p->pcKey,ht2) == NULL) {
	   FInstallKeyHtPRec(p->pcKey,htDiff,p->pRecord);
	 }
       }
     }
   }
   return htDiff;
 };
 
 HASHTABLE HtPlusHtHt(HASHTABLE ht1,HASHTABLE ht2) {
   /* returns the first hash table after adding to it
      all the items in the second */
   int i = 0;
   phtrec p;

   for (p = ht2[i]; i < HASHSIZE ; p = ht2[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* unconditionally insert into result */
	 FInstallKeyHtPRec(p->pcKey,ht1,p->pRecord);
       }
     }
   }
   return ht1;
 };
 
 HASHTABLE HtInterHtHt(HASHTABLE ht1,HASHTABLE ht2) {
   /* returns a hash table representing the set
      intersection of two hash tables */
   int i = 0;
   phtrec p;
   HASHTABLE htInter = HtInit();

   for (p = ht1[i]; i < HASHSIZE ; p = ht1[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* test and possibly insert in result */
	 if (PPRLookupKeyHt(p->pcKey,ht2) != NULL) {
	   FInstallKeyHtPRec(p->pcKey,htInter,p->pRecord);
	 }
       }
     }
   }
   return htInter;
 };
 
 HASHTABLE HtUnionHtHt(HASHTABLE ht1,HASHTABLE ht2) {
   /* returns a hash table representing the set
      union of two hash tables */
   int i = 0;
   phtrec p;
   HASHTABLE htUnion = HtInit();

   for (p = ht1[i]; i < HASHSIZE ; p = ht1[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* unconditionally insert in result */
	 FInstallKeyHtPRec(p->pcKey,htUnion,p->pRecord);
       }
     }
   }
   for (p = ht2[i]; i < HASHSIZE ; p = ht2[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* unconditionally insert in result */
	 FInstallKeyHtPRec(p->pcKey,htUnion,p->pRecord);
       }
     }
   }
   return htUnion;
 };
 
 HTLINKEDLIST HtllFromHt(HASHTABLE ht) {
   /* returns a linked list of hash-table records, containing
      all the members of a hash table; the format is
      that of a one-bucket hash table */
   int i = 0;
   phtrec p;
   phtrec pNew;
   HTLINKEDLIST htlResult = NULL;

   for (p = ht[i]; i < HASHSIZE ; p = ht[++i]) {
     if (p != NULL) {
       for ( ; p != NULL ; p = p->prNext) {
	 /* unconditionally insert in result */
	 pNew = (phtrec) malloc(sizeof(*pNew));
	 if (pNew == NULL)
	   return NULL;
	 pNew->prNext = (phtrec) htlResult;
	 pNew->pcKey = p->pcKey;
	 pNew->pRecord = p->pRecord;
	 htlResult = (HTLINKEDLIST) pNew;
       }
     }
   }
   return htlResult;
 };


#ifdef DEBUGGING
void PrintHashtable(void) {
    phtrec p;
    int i = 0;

    if ( ! fDebug) return;

    fprintf(stderr,"Printing hash table ...\n");
    for (p = ht[0]; i < HASHSIZE ; p = ht[i++]) {
         if (p != NULL) {
              for ( ; p != NULL ; p = p->prNext) {
                   fprintf(stderr,"%d.  %s (%d)\n"
                        i,p->pcKey, p->iValue);
              }
         }
    }
}
#endif

