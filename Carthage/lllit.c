/* Linked-list-literal functions */
/* for DPP (DTD pre-processor) parser
** Revisions:
** 1994-08-19 : CMSMcQ : I *think* i've found the bugs ...
** 1994-08-16 : CMSMcQ : made file
*/

/* The DTD pre-processor needs to accumulate literal strings and use
them in three distinct forms (or maybe only two):

    1 interpreted form (used when installing an entity in the
         entity table)
    2 sgmls output form (sof), with backslash escapes and sof signals
         for parameter entity references and entity ends (used when
         emitting an entity declaration)
    3 uninterpreted form, with or without backslash escaping and/or
         normalization of white space (used in emitting public
         identifiers, system identifiers, and attribute value literals
         used in specifying default values)

Rather that store the string three times, in a record, we will invent an
ad hoc data type, the linked-list literal (LLL), which allows us to read
the literal out in each form, as we need.  An LLL has an anchor,
comprising two pointers (one to the first, one to the last, node; this
speeds up appends), and a list of nodes; each node has a type (literal,
peref, or ee), a value, and a pointer to the next node.

The type provides five operations:
    LLLCreate() returns an empty LLL anchor,
    LLLAppend(lll,kwType,pcValue) appends a node with type and value
         as specified,
    FreeLLL(lll) frees all the memory pointed at or used by the LLL,
    PrintFileLLL(fil,lll) prints the LLL out to a file in sof form,
    PCFromLLL(LLL) returns a pointer to a string concatenating all the
         string nodes
    LLLJoinLL(LLL,LLL) concatenates second LLL to tail of first.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mycat.h"
/* #define DEBUGGING 1
*/

enum kwLLLTYPES {  LLL_LITERAL_NODE,
                   LLL_PEREF_NODE,
                   LLL_EE_NODE
};
typedef struct lllnode * PLLLNode;
typedef struct lllnode {
    enum kwLLLTYPES kwType;       /* what is it? */
    char * pcValue;               /* string, or entname */
    PLLLNode plllNext;            /* next node */
} LLLnode;

typedef struct lllanchor * PLLL;
typedef struct lllanchor {
    PLLLNode pHead;               /* first node in LLL */
    PLLLNode pTail;               /* last node in LLL */
} LLL;

PLLL LLLCreate(void) {
    /* allocate an LLL anchor, initialize it, return it */
    PLLL p;


#ifdef DEBUGGING
    fprintf(stderr,"Entering LLLCreate()\n");
#endif /* DEBUGGING */
    p = (PLLL)malloc(sizeof(*p));
    if (p != NULL) {
         p->pHead = p->pTail = NULL;
         return p;
    } else {
         fprintf(stderr,"Failed to get memory for literal\n");
         exit(16);
    }
}

PLLL LLLAppend(PLLL plll,enum kwLLLTYPES kw,char * pc) {
    /* allocate an LLL node, initialize it, get the tail of the list,
         point tail and anchor->pTail to the new node,
         return anchor */
    PLLLNode pN;
    PLLLNode pT;

#ifdef DEBUGGING
    fprintf(stderr,"Entering LLLAppend(plll,%d,%s)\n",kw,pc);
#endif /* DEBUGGING */
    pN = (PLLLNode)malloc(sizeof(*pN));
    if (pN != NULL) { /* initialize */
         pN->kwType = kw;
         pN->pcValue = pc;
         pN->plllNext = NULL;
    } else {
         fprintf(stderr,"Out of memory trying to adding \"%s\" "
                   "to literal\n",pc);
         exit(16);
    }
    pT = plll->pTail;
    if (pT == NULL) {
#ifdef DEBUGGING
    fprintf(stderr,"This is first append to this llliteral\n");
#endif /* DEBUGGING */
         plll->pHead = plll->pTail = pN;
    } else {
#ifdef DEBUGGING
    fprintf(stderr,"This is not first append to this llliteral\n");
#endif /* DEBUGGING */
         pT->plllNext = plll->pTail = pN;
    }
#ifdef DEBUGGING
    fprintf(stderr,"Have appended new node to this llliteral\n");
    fprintf(stderr,"  type = %d, val=%s\n",
         (plll->pTail)->kwType,
         (plll->pTail)->pcValue);
#endif /* DEBUGGING */
    return plll;
}

PLLL LLLJoinLL(PLLL plllLeft, PLLL plllRight) {

#ifdef DEBUGGING
    fprintf(stderr,"Entering LLLJoinLL(plll,plll)\n");
#endif /* DEBUGGING */
    if (plllLeft->pTail == NULL) { /* Left is empty */
         free(plllLeft);
         return(plllRight);
    } else if (plllRight->pTail == NULL) {  /* right is empty */
         free(plllRight);
         return(plllLeft);
    } else {            /* normal case */
         (plllLeft->pTail)->plllNext = plllRight->pHead;
         plllLeft->pTail = plllRight->pTail;
         free(plllRight);
         return plllLeft;
    }
}

void FreePLLL(PLLL plll) {
    /* traverse the list; at each node, free the string pointed at
         by pcValue, advance one node, free previous node.
         Then free anchor. */
    PLLLNode pCur  = NULL;
    PLLLNode pPrev = NULL;

#ifdef DEBUGGING
    fprintf(stderr,"Entering FreePLLL(plll)\n");
#endif /* DEBUGGING */
    for (pCur = plll->pHead; pCur != NULL ; pCur = pCur->plllNext) {
         /* invariant:  we have freed everything before pPrev */
         free(pPrev);
#ifdef DEBUGGING
    fprintf(stderr,"Freeing string %s\n",pCur->pcValue);
#endif /* DEBUGGING */
         free(pCur->pcValue);
         pPrev = pCur;
    }
    free(plll);
    return;
}

void PrintFileLLL(FILE * pfile, PLLL plll) {
    /* traverse the list; at each node, print appropriate SOF output */
    PLLLNode p;
    char * pc;

#ifdef DEBUGGING
    fprintf(stderr,"Entering PrintFileLLL(plll), start at pHead\n");
#endif /* DEBUGGING */
    for (p = plll->pHead; p != NULL; p = p->plllNext) {
#ifdef DEBUGGING
    fprintf(stderr,"Processing a node:\n  type=%d\n  val =%s\n",p->kwType,p->pcValue);
#endif /* DEBUGGING */
         if (p->kwType == LLL_LITERAL_NODE) {
              pc = myescape(p->pcValue);
              fprintf(pfile,"-%s\n",pc);
              free(pc);
         } else if (p->kwType == LLL_PEREF_NODE) {
              fprintf(pfile,"AENTNAME NAME %s\n(PEREF\n)PEREF\n",
                        p->pcValue);
         } else if (p->kwType == LLL_EE_NODE) {
              fprintf(pfile,"AENTNAME NAME %s\n(EE\n)EE\n",
                        p->pcValue);
         }
    }
    return;
}

char * PCFromPLLL(PLLL plll) {
    /* traverse the list, finding length of interpreted string.
         Allocate that storage, then traverse the list again,
         strcatting each LITERAL node to the output string.
         At end of list, return output string pointer */
    PLLLNode pN;
    char * pc;
    int iLen = 0;

#ifdef DEBUGGING
    fprintf(stderr,"Entering PCFromPLLL(plll)\n");
#endif /* DEBUGGING */
    if (plll == NULL) return NULL;

    /* find cumulative string length */
    for (pN = plll->pHead; pN != NULL; pN = pN->plllNext)
         if (pN->kwType == LLL_LITERAL_NODE)
              iLen += strlen(pN->pcValue);

#ifdef DEBUGGING
    fprintf(stderr,"         PCFromPLLL:  total string length = %d\n",iLen);
#endif /* DEBUGGING */
    pc = (char *)malloc(iLen + 1);
    if (pc == NULL) {
         fprintf(stderr,"Failed getting memory for literal\n");
         exit(16);
    }

/*  pN = plll->pHead;
    strcpy(pc,pN->pcValue);
*/
    *pc = '\0';
    for (pN = plll->pHead; pN != NULL; pN = pN->plllNext)
         if (pN->kwType == LLL_LITERAL_NODE)
              pc = strcat(pc,pN->pcValue);

#ifdef DEBUGGING
    fprintf(stderr,"         PCFromPLLL:  result string = %s\n",pc);
#endif /* DEBUGGING */
    return pc;
}
