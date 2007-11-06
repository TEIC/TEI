/* Hash.H : sample hash-table for string-valued keys.
** In this version, no records:  string is in hash table or not.
*/
/* Revisions:
** 1996-04-24 : CMSMcQ : make header file
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

/* notes for when I allow user-specified size of hash tables 
typedef struct htheader {
  int  * iSize;
  int  * iAdds;
  int  * iDels;
  htrec * phtBody;
};
*/

/* a null prNext pointer marks end of list. */

#define HASHSIZE 1009

/* eventually, we want to allow the user to specify allocation of hash tables
For now we assume a static one called rgRecs
*/

typedef phtrec * HASHTABLE;
typedef phtrec * HTLINKEDLIST;

HASHTABLE HtInit(void);
void DropHt(HASHTABLE ht);
void * PPRLookupKeyHt(char *pc, HASHTABLE ht);
int FInstallKeyHtPRec(char      *pc,
                      HASHTABLE ht,
                      void *    pRec);

/* FInstallKeyHtPRec()
    returns 1 if OK,
    0 if error,
    -1 if entity is already declared / installed
*/

int ICardinalityHt(HASHTABLE ht);
HASHTABLE HtDiffHtHt(HASHTABLE ht1,HASHTABLE ht2);
HASHTABLE HtPlusHtHt(HASHTABLE ht1,HASHTABLE ht2);
HASHTABLE HtInterHtHt(HASHTABLE ht1,HASHTABLE ht2);
HASHTABLE HtUnionHtHt(HASHTABLE ht1,HASHTABLE ht2);
HTLINKEDLIST HtllFromHt(HASHTABLE ht);
