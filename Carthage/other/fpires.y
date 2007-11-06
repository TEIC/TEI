/* fpires.y:  simple processor for formal public identifiers.

** C. M. Sperberg-McQueen

** Revisions:
** 1999-02-21 : CMSMcQ : began work on this, while Lou is burning a CD-ROM
*/

/* to do:
    - find the Oasis spec itself and handle things right
    - allow system identifiers to be URLs (use external call to lynx)
    - first cut at this:  accept comments, PUBLIC, ENTITY, SYSTEM, and CATALOG keywords
*/

/* Known bugs:
** none
*/

/* Known shortcomings other than bugs:
** Not finished
*/

%{
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define myfree(X) free(X)
#define donotfree(X) {}

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#define bcopy(A,B,C) memcpy((B),(A),(C))
#define alloca(A)    malloc(A)
#define max(A,B)  A > B ? A : B
void yyerror(char* s);

#define DEBUGMSG(S) if (fDebug) MsgKwS(msgTRACE,S,NULL)
/*
enum MSGTYPES  { msgTRACE   = 0,
                 msgDEBUG   = 1,
                 msgVERBOSE = 3,
                 msgINFORM  = 5,
                 msgWARNING = 7,
                 msgERROR   = 10
};
*/

#include "mycat.h"
#include "lllit.h"
#include "entmgr.h"
#include "dpplex.h"
#include "msg.h"
#include "hash.h"

%union {
         int   v;
         char* s;
}
%}

%token COM KWPUBLIC KWSYSTEM KWENTITY KWCATALOG
%token NAME LITERAL

%type <s> NAME LITERAL

%%
catalog  : /* */
         | catalog pub
         | catalog sys
         | catalog ent
         | catalog cat
         ;
pub      : KWPUBLIC LITERAL LITERAL {
             printf("Public identifier:\n   '%s'\n   '%s'",$2,$3);
         }
         ;
sys      : KWSYSTEM LITERAL LITERAL {
             printf("System identifier:\n   '%s'\n   '%s'",$2,$3);
         }
         ;
ent      : KWENTITY NAME LITERAL {
             printf("Entity name:\n   '%s'\n   '%s'",$2,$3);
         }
         ;
cat      : KWCATALOG LITERAL {
             printf("Catalog:\n   '%s'\n   '%s'",$2,$3);
         }
         
         ;

%%

void HandleMsglevel(int iMsglevel) {
     fVerbose = fDebug = fTrace = yydebug = 0;
     if (iMsglevel < 4)
       fVerbose = 1;
     if (iMsglevel < 2)
       fDebug = 1;
     if (iMsglevel < 1) {
       fTrace = 1;
       yydebug = 1;
     }
};


void main(int argc, char *argv[]) {
    extern fDebug;
    int argCur;
    int fTemp;
    char * fnRptfile  = NULL;
    int fDeclared = 0, fUsed = 0, fUndeclared = 1, fUnused = 0;

    fnCurrent    = "<stdin>";
    /* get arguments and set things up ... */

    for (argCur = 1; argCur < argc; argCur++) {
      if ((strcmp(argv[argCur],"-?"    ) == 0) ||
          (strcmp(argv[argCur],"-h"    ) == 0) ||
          (strcmp(argv[argCur],"--help") == 0)) {
        fprintf(stderr,"%s:  expected usage is\n",argv[0]);
        fprintf(stderr,"  %s [options] < dtd-file > output-file\n",argv[0]);
        fprintf(stderr,"  where options are these "
                "(* means default):\n");
        fprintf(stderr,"  --msglevel 5*|n (or --trace --debug --verbose)\n");
        fprintf(stderr,"  --msdrop* --mskeep"
                " (drop/keep marked sections, -s0 -s1)\n");
        fprintf(stderr,"  --dupentquiet* --dupentwarn"
                " (warn if entity declared twice? -e0 -e1)\n");
        fprintf(stderr,"  --pedrop* --pekeep"
                " (drop/keep parameter entity declarations -p0 -p1)\n");
        fprintf(stderr,"  --commdrop* --commkeep"
                " (drop/keep parameter entity declarations -c0 -c1)\n");
        fprintf(stderr,"  --undeclared* --unused --declared --used"
                " (include in report file, any or all)\n");
        fprintf(stderr,"  --delete <gi> <gi> ..."
                " (list of GIs to delete from content models)\n");
        fprintf(stderr,"  --delenda <filename>"
                " (name of delete-list file)\n");
        fprintf(stderr,"  --output <filename>"
                " (name of report file, may reuse as delenda file)\n");
        exit(0);
      } else if ((strcmp(argv[argCur],"-m"     ) == 0) ||
                 (strcmp(argv[argCur],"--msglevel") == 0)) {
        { int * pInt;

          sscanf(argv[++argCur],"%d",pInt);
          if (*pInt > 10)
            iMsglevel = 10;
          else if (*pInt < 0)
            iMsglevel = 0;
          else iMsglevel = *pInt;
        }
        HandleMsglevel(iMsglevel);
      } else if (strcmp(argv[argCur],"--trace") == 0) {
        iMsglevel = msgTRACE;
        HandleMsglevel(iMsglevel);
      } else if (strcmp(argv[argCur],"--debug") == 0) {
        iMsglevel = msgDEBUG;
        HandleMsglevel(iMsglevel);
      } else if (strcmp(argv[argCur],"--verbose") == 0) {
        iMsglevel = msgVERBOSE;
        HandleMsglevel(iMsglevel);
      } else if ((strcmp(argv[argCur],"-s0"     ) == 0) ||
                 (strcmp(argv[argCur],"--msdrop") == 0)) {
        fMSIgnore = 1;
      } else if ((strcmp(argv[argCur],"-s1"      ) == 0) ||
                 (strcmp(argv[argCur],"--mskeep") == 0)) {
        fMSIgnore = 0;
      } else if ((strcmp(argv[argCur],"-e0"          ) == 0) ||
                 (strcmp(argv[argCur],"--dupentquiet") == 0)) {
        fEntwarn = 0;
      } else if ((strcmp(argv[argCur],"-e1"         ) == 0) ||
                 (strcmp(argv[argCur],"--dupentwarn") == 0)) {
        fEntwarn = 1;
      } else if ((strcmp(argv[argCur],"-p0"     ) == 0) ||
                 (strcmp(argv[argCur],"--pedrop") == 0)) {
        fKeepPEs = 0;
      } else if ((strcmp(argv[argCur],"-p1"     ) == 0) ||
                 (strcmp(argv[argCur],"--pekeep") == 0)) {
        fKeepPEs = 1;
      } else if ((strcmp(argv[argCur],"-c0"       ) == 0) ||
                 (strcmp(argv[argCur],"--commdrop") == 0)) {
        fKeepComms = 0;
      } else if ((strcmp(argv[argCur],"-c1"       ) == 0) ||
                 (strcmp(argv[argCur],"--commkeep") == 0)) {
        fKeepComms = 1;
      } else if (strcmp(argv[argCur],"--declared") == 0) {
        fDeclared = 1;
      } else if (strcmp(argv[argCur],"--undeclared") == 0) {
        fUndeclared = 1;
      } else if (strcmp(argv[argCur],"--used") == 0) {
        fUsed = 1;
      } else if (strcmp(argv[argCur],"--unused") == 0) {
        fUnused = 1;
      } else if ((strcmp(argv[argCur],"-d"      ) == 0) ||
                 (strcmp(argv[argCur],"--delete") == 0)) {
        while ((++argCur < argc) && (isalpha(*argv[argCur]))) {
          fTemp = FInstallKeyHtPRec(argv[argCur],htDelenda,NULL);
          MsgKwS(msgVERBOSE,"Installed ",argv[argCur]," in delenda",NULL);
          /*
          fprintf(stderr,"Installed %s in delenda\n",argv[argCur]);
          */
        };
        if (argCur < argc) argCur--;
      } else if ((strcmp(argv[argCur],"-f"       ) == 0) ||
                 (strcmp(argv[argCur],"--delenda") == 0)) {
        if (++argCur >= argc) {
          fprintf(stderr,"--delenda option requires filename argument\n");
          exit(4);
        }
        { char * fnDelfile = mycopy1(argv[argCur]);
          FILE * pfDelfile = fopen(fnDelfile,"r");
          if (pfDelfile == NULL) {
            MsgKwS(msgVERBOSE,"Can't open delenda file ",fnDelfile,NULL);
          } else {
            yyin = pfDelfile;
            initfstack();
            lexbegin(CON);
            yyparse();
            fclose(pfDelfile);
            yyin = stdin; /* reattach to normal */
            MsgKwS(msgVERBOSE,"Installed delenda from ",fnDelfile,NULL);
          };
        };
      } else if ((strcmp(argv[argCur],"-o"      ) == 0) ||
                 (strcmp(argv[argCur],"--output") == 0)) {
        if (++argCur >= argc) {
          fprintf(stderr,"--delenda option requires filename argument\n");
          exit(4);
        }
        fnRptfile = mycopy1(argv[argCur]);
      } else if (argv[argCur][0] == '-') {
        fprintf(stderr,"%s:  unknown option %s\n",argv[0],argv[argCur]);
      } else {
        fprintf(stderr,"%s:  sorry, no filename arguments yet\n",argv[0]);
        /*
        MsgKwS(msgWARNING,
               "Sorry, filename arguments not implemented yet",NULL);
               */
      }
    };

    initfstack();
    lexbegin(CON);
    yyparse();

}

void yyerror(char* s) {
    fprintf(stdout,"L%d %s <-- %s -->\n",cLinecount,fnCurrent,s);
    fprintf(stderr,"! ! ! line %d of %s:  %s ! ! !\n",
              cLinecount,fnCurrent,s);
}

char * PCExtid (struct rEnttext *p) {
    if ((p->pcSys == NULL) && (p->pcPub == NULL)) {
      return mycopy1("SYSTEM");
    } else if ((p->pcSys == NULL) && (p->pcPub != NULL)) {
      return mycat3("PUBLIC \"",p->pcPub,"\"");
    } else if ((p->pcSys != NULL) && (p->pcPub == NULL)) {
      return mycat3("SYSTEM \"",p->pcSys,"\"");
    } else if ((p->pcSys != NULL) && (p->pcPub != NULL)) {
      return mycat5("PUBLIC \"",p->pcPub,
                    "\" \"",p->pcSys,
                    "\"");
    } else {
      /* rules of logic no longer apply */
      return mycopy1("");
    }
};

