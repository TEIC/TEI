/* lllit.h:  headers for linked-list-literal functions */

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

PLLL LLLCreate(void);
PLLL LLLAppend(PLLL plll,enum kwLLLTYPES kw,char * pc);
PLLL LLLJoinLL(PLLL plllLeft, PLLL plllRight);
void FreePLLL(PLLL plll);
void PrintFileLLL(FILE * pfile, PLLL plll);
char * PCFromPLLL(PLLL plll);
