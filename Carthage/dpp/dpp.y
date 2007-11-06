/* dpp.y:  DTD pre-processor.  Reads DTD, emits sgmls data stream */

/* The sgmls data stream is for a virtual SGML document instance
** conforming to dtd.dtd and specifying the same document grammar. */

/* to do:
    - add ps to DTD and to grammar here (to allow in-decl comments)
    - add error diagnosis (not nec. recovery)
    - add more SGML checking (identifier uniqueness, etc.)
    - enforce parameter entity start-end rules?
    - complete the grammar, by adding
         * ranked elements, ranked groups
         * data tag groups
         * short-reference maps and short-reference use declarations
         * ? formal public identifier checking ?
    - add argument processing (allow series of file ids)
*/

/* begun  2 Aug 94, based on old p2dtd.y

**
** This grammar of the SGML DTD language now covers only the
** partial language used in TEI DTDs and the constructs allowed in the
** TEI interchange format.  For simplicity, we do not enforce all the
** rules about entity start and end and spaces.
**
** Revisions:
** 31 Mar 95 correct gis for EXCEPTNS, EXCL, INCL
** 20 Jan 95 allow input stream to contain just dtdsubset (i.e. 
**            don't require a document type declaration)
** 19 Aug 94 finally fixed bug in literals with linked-list-literal
**            structure
**           - add special rule for exceptions, to accommodate cases
**            like -(a,b)+(c,d) with no space between excl, incl
**           - fix dtd.dtd decl for literal (#pcdata | peref | ee)*
**           - added rules for marked sections, then actions
** 16 Aug 94 minor changes connected with getting entity refs to work
**           move myescape() calls here from lex
**           change LITERAL to literalstring (selectively)
** 14 Aug 94 unquote internal enttext, pubid, sysid before installing
**            entities in table
** 13 Aug 94 call FInstallEntity() from entity decl action
** 13 Aug 94 revamp entity declarations, to allow us to call the
**            entity manager installation procedure (need to have
**            identifiable sysid, pubid, internal string, etc. at
**            the level of ent decl, i.e. extid and enttype must
**            return a structure (here:  rEnttext)
** 12 Aug 94 added entity manager with install procedure
**  8 Aug 94 added option processing
**           also added error rule to dtdsubset (resume processing
**           after next MDC, emit error msg)
**  6 Aug 94 CMSMcQ complete elements, pi, comm, notatn, attlist,
**                  fix name-scanning problem (is it fixed?)
**  2 Aug 94 CMSMcQ begin with old p2dtd.y and p2dtdlex.l files
*/

/* Known bugs:
- This parser fails to allow unrecognized parameter entity references
  in system identifiers and in attribute value literals given as
  default values in attribute definitions.
  In a conforming parser, pero should not be recognized within
  attribute value specifications, system identifiers, or public
  identifiers.  The parser does forbid pero + letter correctly
  within public identifiers, but is wrong to allow pero + nonletter.
  (Need semantic checks on all three forms of literal.  When we have
  them, allow false recognition within attribute value literals
  and sysid).
*/
/* Known non-bug shortcomings:
- ps is silently eaten by lexical scanner, instead of being presented
  to output stream
*/

%{
#define YYDEBUG 1
#define YYERROR_VERBOSE 1

#include <string.h>
/* #include <stdlib.h> */
#include <stdio.h>

#define bcopy(A,B,C) memcpy((B),(A),(C))
#define alloca(A)    malloc(A)
#define max(A,B)  A > B ? A : B
void yyerror(char* s);
#define DEBUGMSG(S) if (fDebug) MsgKwS(msgDEBUG,S,NULL)
enum MSGTYPES  { msgTRACE   = 0,
                 msgDEBUG   = 1,
                 msgVERBOSE = 3,
                 msgINFORM  = 5,
                 msgWARNING = 7,
                 msgERROR   = 10
};

char *pcT;
char *pcX;
char *pcT1;
char *pcT2;
char *pcT3;
char *pcT4;
int  i, j, k, iLen;
int f;
int fTrace = 0;
int fDebug = 0;
int fVerbose = 0;
int fMSIgnore = 1;
int fEntwarn = 0;
int iMsglevel = msgINFORM;

#include "mycat.h"
#include "lllit.h"
#include "entmgr.h"
#include "dpplex.h"
#include "msg.h"

char *myDvKey(char *pKeyword);

/* stuff for entity declarations ... */
enum kwENTTYPES {  kwNILTYPE,
                   kwCDATA, kwSDATA, kwPI,
                   kwSTARTTAG, kwENDTAG, kwMS, kwMD,
                   kwSUBDOC, kwNDATA,
                   kwNORMAL
};
struct rEnttext {
    int  fExt;               /* is it an external entity? */
    enum kwENTTYPES kwType;  /* NORMAL | CDATA | SDATA | ... | SUBDOC */
    PLLL plllInt;            /* internal string, as linked-list literal */
    char *pcPub;             /* public identifier, or null */
    char *pcSys;             /* system identifier, or null */
    char *pcNotn;            /* notation name, or null */
    char *pcAtts;            /* attribute list (sof form) or null */
    /* for dpp, we leave pcAtts in sgmls output form for dtd.dtd.
    In a full parser, this will need a special record type */
};
struct rEnttext *pRENew(void);
struct rEnttext *myEntLit(enum kwENTTYPES kw, PLLL plll);
char * PCExtid (struct rEnttext *p);
void PrintFileEnttext (FILE * pfile, struct rEnttext *p);
void FreemembersRE (struct rEnttext *p);

/* for marked sections */
enum kwMSTYPES { MS_TEMP, MS_INCLUDE, MS_RCDATA, MS_CDATA, MS_IGNORE };
static char *rgKwms[] = { "TEMP","INCLUDE","RCDATA","CDATA","IGNORE" };
char * PCDoMskw(enum kwMSTYPES kw);
%}

%union {
         int   v;
         char* s;
         struct rEnttext *e;
         PLLL lllit;
}

/* All token types given here correspond to literals in the BNF, except
**   'Token types'
*/
/* Delimiters, including the anomalous PERO + SPACE */
%token MDC GRPO GRPC DSO DSC COM PIO PIC VI
%token PEROSPACE LITDELIM
%token MDODSO MSCMDC

/* Compound keywords */
%token MDELEMENT MDDOCTYPE MDOCOM MDOMDC MDATTLIST
%token MDENTITY  MDNOTATION

/* Operators for minimization, content models, and exceptions */
%token AND OR SEQ OPT PLUS REP MINUS OH
%token GRPOPT GRPREP GRPPLUS PLUSGRP MINUSGRP

/* Keywords */
%token KWCDATA KWRCDATA KWEMPTY KWANY
%token RNIPCDATA KWSYSTEM KWPUBLIC
%token RNINOTATION KWENTITY KWENTITIES KWID KWIDREF KWIDREFS
%token KWNAME KWNAMES KWNMTOKEN KWNMTOKENS KWNUMBER KWNUMBERS
%token KWNUTOKEN KWNUTOKENS KWNOTATION
%token KWSDATA KWPI KWSTAG KWETAG KWMS KWMD KWNDATA KWSUBDOC
%token KWTEMP KWINCLUDE KWIGNORE

%token RNIFIXED RNIREQUIRED RNICURRENT RNICONREF RNIIMPLIED
%token RNIDEFAULT

/* Parameter Entity References */
%token PEREF EE PEREFERROR
/* Token types */
%token NAME LITERAL STRING NUMBER NUMTOK

%type <s> NAME STRING LITERAL NUMBER NUMTOK
%type <s> KWCDATA KWRCDATA KWEMPTY KWANY
%type <s> KWSYSTEM KWPUBLIC OH
%type <s> KWENTITY KWENTITIES KWID KWIDREF KWIDREFS
%type <s> KWNAME KWNAMES KWNMTOKEN KWNMTOKENS KWNUMBER KWNUMBERS
%type <s> KWNUTOKEN KWNUTOKENS KWNOTATION
%type <s> KWSDATA KWPI KWSTAG KWETAG KWMS KWMD KWNDATA KWSUBDOC
%type <s> PEREF EE PEREFERROR
%type <s> elementdecl commdecl commseq procinst attlistdecl
%type <s> elemtype contentdecl namegrp andnames ornames seqnames
%type <s> model tokengrp occurrence andtokens ortokens seqtokens token
%type <s> minimiz min
%type <s> exceptions inclusions exclusions
%type <s> associated attdeflist attdef valtype default value
%type <s> assocnotatn
%type <s> nmtokgrp nmtokseq nmtokcom nmtokbar nmtokamp nametoken
%type <s> attspeclist attspec
%type <s> sysid pubid namesym numtoken
%type <s> markedsection mksecstart mskeywords mskeyword
%type <e> extid externdtd enttext enttype
%type <v> enttypekw
%type <lllit> litstring litdata

%%

prolog   : misc dtdseq                           /* cf. 7, 9 */
         | misc nonmisc dtdsubset
         ;

/* nonmisc is everything in a DTD subset except:
   commdecl, procinst (which are in misc)
   peref, errorpe (which cannot usefully be the first non-misc item)
*/
nonmisc  : entitydecl   { DEBUGMSG("read entity decl"); }
         | elementdecl  { DEBUGMSG("read elem decl"); }
         | attlistdecl  { DEBUGMSG("read attlist decl"); }
         | notndecl     { DEBUGMSG("read notation decl"); }
         | markedsection { DEBUGMSG("read MS decl"); }
         | error MDC {
              MsgKwS(msgERROR,"Error parsing a declaration here",NULL);
         }
         ;

dtdseq   : dtd misc                              /* cf. 7 */
         | dtdseq dtd misc
         ;
dtd      : MDDOCTYPE dtdname externdtd dso dtdsubset dsc exmdc
           /* cf. 110 */
         | MDDOCTYPE dtdname           dso dtdsubset dsc MDC {
              printf(")DTD\n)PROLOG\n");
         }
         | MDDOCTYPE dtdname externdtd                   exmdc
         | MDDOCTYPE dtdname                             MDC {
              /* code for external DTD file here ... */
              /* none yet.  In the meantime ... */
              MsgKwS(msgWARNING,"Vacuous DTD:  no external DTD entity, "
                             "no DTD subset",NULL);
              printf(")DTD\n)PROLOG\n");
         }
         ;
dtdname  : namesym {
              printf("(PROLOG\n");
              printf("(DTD\n");
              printf("(DOCTYPE\n-%s\n)DOCTYPE\n", $1);
/*            free($1);  */
         }        /* cf. 111 */
         ;
externdtd: extid { /* external identifier for DTD */
              pcT = PCExtid($1);
              printf("%s",pcT);
              free(pcT);

              pcT  = mycopy1("#extDTD");
              pcT1 = NULL;
              pcT2 = $1->pcPub;
              pcT3 = $1->pcSys;
              f = FInstallEntity(pcT,pcT1,1,pcT2,pcT3);
              if (f == 0) {
                   MsgKwS(msgERROR,
                      "Error adding external DTD file to table",NULL);
                   exit(8);
              } else if (f == -1) {
                   MsgKwS(msgERROR,
                      "External DTD file already declared",NULL);
              }
              FreemembersRE($1);
              free($1);
         }
         ;
dso      : DSO {   /* opening of doc type decl subset */
              printf("(DSUBSET\n");
         }
         ;
dsc      : DSC {   /* closing of doc type decl subset */
              printf(")DSUBSET\n");
         }
         ;
exmdc    : MDC {   /* end of doc type decl with external DTD file */
                   printf("(DEXTERN\n");
                   /* code for external DTD file here ... */
                   if (FOpenEntity("#extDTD")) {
                        lexbegin(DS);
                   } else { MsgKwS(msgERROR,
                             "Could not open external DTD file",NULL);
                   }
              }
           dtdsubset EE {
                   printf(")DEXTERN\n)DTD\n)PROLOG\n");
         }
         ;
dtdsubset : /* */                                /* cf. 112-114 */
         | dtdsubset entitydecl   { DEBUGMSG("read entity decl"); }
         | dtdsubset elementdecl  { DEBUGMSG("read elem decl"); }
         | dtdsubset attlistdecl  { DEBUGMSG("read attlist decl"); }
         | dtdsubset notndecl     { DEBUGMSG("read notation decl"); }
         | dtdsubset commdecl     { DEBUGMSG("read comment decl"); }
         | dtdsubset procinst     { DEBUGMSG("read PI"); }
         | dtdsubset pereference  { DEBUGMSG("ended PE ref"); }
         | dtdsubset errorpe      { DEBUGMSG("error in PE ref"); }
         | dtdsubset markedsection { DEBUGMSG("read MS decl"); }
         | dtdsubset error MDC {
              MsgKwS(msgERROR,"Error parsing a declaration here",NULL);
         }
         ;

/********************************************************************/
/* Element Declarations                                             */
/********************************************************************/
elementdecl : MDELEMENT elemtype minimiz contentdecl MDC { /* cf. 116 */
              printf("(ELEMENT\n%s%s%s)ELEMENT\n",$2,$3,$4);
/*            free($2); free($3); free($4); */
         }
         ;

elemtype : namesym {                                /* cf. 117 */
              $$ = mycat3("(ELEMTYPE\n(GI\n-",$1,"\n)GI\n)ELEMTYPE\n");
              free($1);
         }
         | GRPO namegrp GRPC {
              $$ = mycat3("(ELEMTYPE\n",$2,")ELEMTYPE\n");
         }
         ;

minimiz  : /* nil */ {                           /* cf. 122-24 */
              $$ = mycopy1("");
         }
         | min min {                             /* cf. 122-24 */
              $$ = mycat5("(OMISS\n-",$1," ",$2,"\n)OMISS\n");
         }
         ;
min      : MINUS                { $$ = "-"; }
         | OH                   { $$ = "O"; }
         ;


contentdecl : KWCDATA  { $$ = mycopy1("(CDATA\n)CDATA\n"); } /* 125f */
         | KWRCDATA    { $$ = mycopy1("(RCDATA\n)RCDATA\n"); }
         | KWEMPTY     { $$ = mycopy1("(EMPTY\n)EMPTY\n"); }
         | KWANY exceptions    {
              $$ = mycat3("(MODEL\n(ANY\n)ANY\n",$2,")MODEL\n");
              free($2);
         }
         | model  exceptions    {
              $$ = mycat4("(MODEL\n",$1,$2,")MODEL\n");
              free($1); free($2);
         }
         ;

/* content model group nests, unlike other groups.  Cf. 127 */
model    : GRPO tokengrp GRPC {
              $$ = mycat2("AOCC TOKEN NIL\n",$2);
              free($2);
         }
         | GRPO tokengrp GRPOPT {
              $$ = mycat2("AOCC TOKEN OPT\n",$2);
              free($2);
         }
         | GRPO tokengrp GRPREP {
              $$ = mycat2("AOCC TOKEN REP\n",$2);
              free($2);
         }
         | GRPO tokengrp GRPPLUS {
              $$ = mycat2("AOCC TOKEN PLUS\n",$2);
              free($2);
         }
         ;
tokengrp : seqtokens {                           /* cf. 127 */
              $$ = mycat3("ACONN TOKEN SEQ\n(MODELGRP\n",
                        $1,")MODELGRP\n");
              free($1);
         }
         | ortokens {
              $$ = mycat3("ACONN TOKEN OR\n(MODELGRP\n",
                        $1,")MODELGRP\n");
              free($1);
         }
         | andtokens {
              $$ = mycat3("ACONN TOKEN AND\n(MODELGRP\n",
                        $1,")MODELGRP\n");
              free($1);
         }
         ;
seqtokens : token {
              $$ = $1;
         }
         | seqtokens SEQ token {
              $$ = mycat2($1,$3);
              free($1); free($3);
         }
         ;
ortokens : token OR token {
              $$ = mycat2($1,$3);
              free($1); free($3);
         }
         | ortokens OR token {
              $$ = mycat2($1,$3);
              free($1); free($3);
         }
         ;
andtokens : token AND token {
              $$ = mycat2($1,$3);
              free($1); free($3);
         }
         | andtokens  AND token {
              $$ = mycat2($1,$3);
              free($1); free($3);
         }
         ;
token    : RNIPCDATA {                           /* cf. 128-130 */
              $$ = mycopy1("(PCDATA\n)PCDATA\n");
         }
         | namesym occurrence {
              /* n.b. ISO 8879 forbids space after name,
                 but we don't enforce that here. */
              $$ = mycat5("AOCC TOKEN ",$2,
                             "\n(ETOKEN\n-",$1,"\n)ETOKEN\n");
              free($1);
         }
         | model { $$ = $1; }
         ;
occurrence : /* empty */ { $$ = "NIL";          }   /* cf. 132 */
         | OPT        { $$ = "OPT"; }
         | PLUS       { $$ = "PLUS"; }
         | REP        { $$ = "REP"; }
         ;

exceptions : exclusions inclusions {             /* cf. 138-40 */
              if ((strlen($1) + strlen($2)) > 0) {
                   $$ = mycat4("(EXCEPTNS\n",
                                  $1,$2,")EXCEPTNS\n");
                   free($1); free($2);
              }
         }
         | MINUSGRP namegrp GRPPLUS GRPO namegrp GRPC {
              /* lexx misreads -(a,b)+(c,d) so we need special case */
              $$ = mycat5("(EXCEPTNS\n(EXCL\n",$2,
                          ")EXCL\n(INCL\n",$5,
                          ")INCL\n)EXCEPTNS\n");

         }
         ;
exclusions : /* empty */          { $$ = mycopy1(""); }
         | MINUSGRP namegrp GRPC  {
              $$ = mycat3("(EXCL\n",$2,")EXCL\n");
              free($2);
         }
         ;
inclusions : /* empty */          { $$ = mycopy1(""); }
         | PLUSGRP namegrp GRPC   {
              $$ = mycat3("(INCL\n",$2,")INCL\n");
              free($2);
         }
         ;

/*
exceptions : ** nil **                           ** cf. 138-40 **
         | PLUSGRP    { printf("(EXCEPTNS\n(INCL\n"); }
         namegrp GRPC { printf(")INCL\n)EXCEPTNS\n"); }
         | MINUSGRP   { printf("(EXCEPTNS\n(EXCL\n"); }
         namegrp GRPC { printf(")EXCL\n)EXCEPTNS\n"); }
         | MINUSGRP   { printf("(EXCEPTNS\n(EXCL\n"); }
         namegrp GRPC { printf(")EXCL\n"); }
         PLUSGRP      { printf("(INCL\n"); }
         namegrp GRPC { printf(")INCL\n)EXCEPTNS\n"); }
         ;
*/

/********************************************************************/
/* AttList Declarations                                             */
/********************************************************************/
attlistdecl : MDATTLIST associated attdeflist MDC { /* Cf. 141 */
              printf("(ATTLIST\n%s(ATTDEFS\n%s)ATTDEFS\n)ATTLIST\n",
                   $2,$3);
/*            free($2); free($3); */
         }
         ;
associated : elemtype
         | assocnotatn
         ;

attdeflist : attdef                         /* Cf. 142 */
         | attdef attdeflist {
              $$ = mycat2($1,$2); free($1); free($2);
         }
         ;

attdef   : namesym valtype default {           /* Cf. 143-44 */
              pcT = mycat5("(ATTDEF\n(NAME\n-",$1,"\n)NAME\n",$2,$3);
              $$ = mycat2(pcT,")ATTDEF\n");
              free($1); free($2); free($3); free(pcT);
         }
         ;

valtype  : KWCDATA      { $$ = myDvKey("CDATA");}/* cf. 145 */
         | KWENTITY     { $$ = myDvKey("ENTITY");   }
         | KWENTITIES   { $$ = myDvKey("ENTITIES"); }
         | KWID         { $$ = myDvKey("ID");       }
         | KWIDREF      { $$ = myDvKey("IDREF");    }
         | KWIDREFS     { $$ = myDvKey("IDREFS");   }
         | KWNAME       { $$ = myDvKey("NAME");     }
         | KWNAMES      { $$ = myDvKey("NAMES");    }
         | KWNMTOKEN    { $$ = myDvKey("NMTOKEN");  }
         | KWNMTOKENS   { $$ = myDvKey("NMTOKENS"); }
         | KWNUMBER     { $$ = myDvKey("NUMBER");   }
         | KWNUMBERS    { $$ = myDvKey("NUMBERS");  }
         | KWNUTOKEN    { $$ = myDvKey("NUTOKEN");  }
         | KWNUTOKENS   { $$ = myDvKey("NUTOKENS"); }
         | KWNOTATION GRPO namegrp GRPC {
              $$ = mycat3("(DVALUE\n(DVNOTN\n",$3,")DVNOTN\n)DVALUE\n");
              free($3);
         }
         | nmtokgrp     { 
              $$ = mycat3("(DVALUE\n",$1,")DVALUE\n");
              free($1);
         }
         ;

nmtokgrp : GRPO nmtokseq GRPC { $$ = $2; }      /* 68, 131 */
         ;
nmtokseq : nmtokcom {
              $$ = mycat3("ACONN TOKEN SEQ\n(NTGRP\n",$1,")NTGRP\n");
              free($1);
         }
         | nmtokbar {
              $$ = mycat3("ACONN TOKEN OR\n(NTGRP\n",$1,")NTGRP\n");
              free($1);
         }
         | nmtokamp {
              $$ = mycat3("ACONN TOKEN AND\n(NTGRP\n",$1,")NTGRP\n");
              free($1);
         }
         ;
nmtokcom : nametoken {
              $$ = mycat3("(NTOKEN\n-",$1,"\n)NTOKEN\n");
              free($1);
         }
         | nmtokcom SEQ nametoken {
              $$ = mycat4($1,"(NTOKEN\n-",$3,"\n)NTOKEN\n");
              free($1); free($3);
         }
         ;
nmtokbar : nametoken OR nametoken {
              $$ = mycat5("(NTOKEN\n-",$1,"\n)NTOKEN\n(NTOKEN\n-",
                             $3,"\n)NTOKEN\n");
              free($1); free($3);
         }
         | nmtokbar  OR nametoken {
              $$ = mycat4($1,"(NTOKEN\n-",$3,"\n)NTOKEN\n");
              free($1); free($3);
         }
         ;
nmtokamp : nametoken AND nametoken {
              $$ = mycat5("(NTOKEN\n-",$1,"\n)NTOKEN\n(NTOKEN\n-",
                             $3,"\n)NTOKEN\n");
              free($1); free($3);
         }
         | nmtokamp  AND nametoken {
              $$ = mycat4($1,"(NTOKEN\n-",$3,"\n)NTOKEN\n");
              free($1); free($3);
         }
         ;

default  :  value {                          /* Cf. 147 */
              $$ = mycat3("(DEFAULT\n(AVSPEC\n-",
                        $1,"\n)AVSPEC\n)DEFAULT\n");
              free($1);
         }
         | RNIFIXED value {
              $$ = mycat3("(DEFAULT\n(FIXED\n)FIXED\n(AVSPEC\n-",
                        $2,"\n)AVSPEC\n)DEFAULT\n");
              free($2);
         }
         | RNIREQUIRED {
              $$ = mycopy1("(DEFAULT\n(REQUIRED\n)REQUIRED\n)DEFAULT\n");
         }
         | RNICURRENT {
              $$ = mycopy1("(DEFAULT\n(CURRENT\n)CURRENT\n)DEFAULT\n");
         }
         | RNICONREF {
              $$ = mycopy1("(DEFAULT\n(CONREF\n)CONREF\n)DEFAULT\n");
         }
         | RNIIMPLIED {
              $$ = mycopy1("(DEFAULT\n(IMPLIED\n)IMPLIED\n)DEFAULT\n");
         }
         ;

assocnotatn : RNINOTATION namesym {                 /* Cf. 149.1 */
              $$ = mycat3("(NOTNAME\n(NAME\n-",$2,"\n)NAME\n)NOTNAME\n");
              free($2);
         }
         | RNINOTATION GRPO namegrp GRPC {
              $$ = mycat3("(NOTNAME\n",$3,")NOTNAME\n");
              free($3);
         }
         ;

/********************************************************************/
/* Notation Declarations                                            */
/********************************************************************/
notndecl : MDNOTATION namesym extid MDC {
              pcT = PCExtid($3);
              printf("(NOTATION\n(NAME\n-%s\n)NAME\n",$2);
              printf("%s)NOTATION\n",pcT);
              FreemembersRE($3); free(pcT);
              free($2); free($3);
         }
         ;

/********************************************************************/
/* Entity Declarations                                              */
/********************************************************************/
/* entity set */
/* need to replace enttext string with call to myentlit, or ... */
entitydecl : MDENTITY  namesym enttext MDC {   /* cf. 101-104 */
              printf("ATYPE TOKEN GE\n(ENTITY\n"
                   "(ENTNAME\n-%s\n)ENTNAME\n",$2);
              PrintFileEnttext(stdout,$3);
              printf(")ENTITY\n");
              FreemembersRE($3);
              free($2); free($3);
         }
         | MDENTITY  RNIDEFAULT enttext MDC {
              printf("ATYPE TOKEN GE\n(ENTITY\n"
                   "(ENTNAME\n-#DEFAULT\n)ENTNAME\n");
              PrintFileEnttext(stdout,$3);
              printf(")ENTITY\n");
              FreemembersRE($3);
              free($3);
         }
         | MDENTITY  PEROSPACE  namesym enttext MDC {
              printf("ATYPE TOKEN PE\n(ENTITY\n"
                   "(ENTNAME\n-%s\n)ENTNAME\n",$3);
              PrintFileEnttext(stdout,$4);
              printf(")ENTITY\n");
              MsgKwS(msgDEBUG,"<!ENTITY % ",$4," \"...\">",NULL);
              /* load into table here */
              /*
              pcT1 = PCUnquotePC($4->pcInt);
              pcT2 = PCUnquotePC($4->pcPub);
              pcT3 = PCUnquotePC($4->pcSys);
              */
              pcT1 = PCFromPLLL($4->plllInt);
              pcT2 = $4->pcPub;
              pcT3 = $4->pcSys;
              f = FInstallEntity($3,pcT1,
                   $4->fExt,pcT2,pcT3);
              if (f == 0) {
                   MsgKwS(msgERROR,"Error adding entity ",$3," to table",NULL);
                   exit(8);
              } else if (f == -1) {
                   MsgKwS((fEntwarn ? msgWARNING : msgDEBUG),
                   "Entity ",$3," already declared",NULL);
              }
              if (fVerbose) {
                   MsgKwS(msgDEBUG,"Entity %% ",$3," has been installed",
                          NULL);
              }
              FreemembersRE($4); free(pcT1);
              free($3); free($4);
         }
         ;

enttext  : litstring  {                          /* cf. 105-108 */
              $$ = myEntLit(kwNORMAL,$1);
         }
         | KWCDATA    litstring { $$ = myEntLit(kwCDATA,$2);    }
         | KWSDATA    litstring { $$ = myEntLit(kwSDATA,$2);    }
         | KWPI       litstring { $$ = myEntLit(kwPI,$2);       }
         | KWSTAG     litstring { $$ = myEntLit(kwSTARTTAG,$2); }
         | KWETAG     litstring { $$ = myEntLit(kwENDTAG,$2);   }
         | KWMS       litstring { $$ = myEntLit(kwMS,$2);       }
         | KWMD       litstring { $$ = myEntLit(kwMD,$2);       }
         | extid enttype {
              $$ = $2;
              $$->fExt = 1;
              $$->pcPub = $1->pcPub;
              $$->pcSys = $1->pcSys;
              free($1);
/*
              pcT = PCExtid($1);
              $$ = mycat4("(EXTERNAL\n",pcT,$2,")EXTERNAL\n");
*/
         }
         ;

enttype  : /* */ {                          /* cf. 108-109, 149.2 */
              $$ = pRENew(); $$->kwType = kwNORMAL;
         }
         | KWSUBDOC     {
              $$ = pRENew(); $$->kwType = kwSUBDOC;
         }
         | enttypekw namesym {
              $$ = pRENew(); $$->kwType = $1; $$->pcNotn = $2;
         }
         | enttypekw namesym DSO attspeclist DSC {
              $$ = pRENew(); $$->kwType = $1; $$->pcNotn = $2;
              $$->pcAtts = $4;
         }
         ;

enttypekw: KWCDATA { $$ = kwCDATA; free($1); }
         | KWNDATA { $$ = kwNDATA; free($1); }
         | KWSDATA { $$ = kwSDATA; free($1); }
         ;

extid    : KWSYSTEM       {
              $$ = pRENew();
         }
         | KWSYSTEM sysid {
              $$ = pRENew();
              $$->pcSys = $2;
         }
         | KWPUBLIC pubid {
              $$ = pRENew();
              $$->pcPub = $2;
         }
         | KWPUBLIC pubid sysid {
              $$ = pRENew();
              $$->pcSys = $3; $$->pcPub = $2;
         }
         ;
sysid    : litstring { $$ = PCFromPLLL($1); }
         ;

/********************************************************************/
/* Common Constructs                                                */
/********************************************************************/
/* Common Constructs */
misc     : /* nothing */                         /* cf. 8 */
         | misc commdecl
         | misc procinst
         ;
commdecl : MDOCOM STRING COM commseq MDC {       /* cf. 91-92 */
              pcT = myescape($2);
              printf("(COMMDECL\n(COMMENT\n-%s%s"
                        "\n)COMMENT\n)COMMDECL\n",pcT,$4);
              MsgKwS(msgDEBUG,"<!-- ",pcT,$4," -->",NULL);
              free($2); free($4); free(pcT);
         }
         | MDOMDC {
                printf("(COMMDECL\n)COMMDECL\n");
         }
         ;
commseq  : /* nothing */ {
              $$ = mycopy1("");
         }
         | commseq COM STRING COM {
              $$ = mycat4($1,"(COMMENT\n",$3,")COMMENT\n");
              free($1); free($3);
         }
         ;
procinst : PIO STRING PIC {                      /* cf. 44 */
              pcT = myescape($2);
              printf("(PI\n-%s\n)PI\n",pcT);
              free($2); free(pcT);
         }
         ;

litstring : LITDELIM litdata LITDELIM { $$ = $2;
              if (fDebug) {
                   pcT = PCFromPLLL($2);
                   MsgKwS(msgDEBUG,"Literal string recognized:  ",pcT,NULL);
              }
         }
         ;
litdata : /* nil */   { $$ = LLLCreate(); }
         | litdata LITERAL {
              $$ = LLLAppend($1,LLL_LITERAL_NODE,$2);
/*            pcT = myescape($2);
              $$ = mycat2($1,pcT);
              free($1); free($2); free(pcT);
*/
         }
         | litdata PEREF litdata EE {
              $$ = LLLAppend($1,LLL_PEREF_NODE,$2);
              $$ = LLLJoinLL($$,$3);
              $$ = LLLAppend($$,LLL_EE_NODE,$4);
/*            pcT = mycat3("AENTNAME NAME",
                             $2,"\n(PEREF\n)PEREF\n");
              if (($4 != NULL) && (strlen($4) > 0))
                   pcX = mycat3("AENTNAME NAME",$2,"\n(EE\n)EE\n");
              else pcX = mycopy1("(EE\n)EE\n");
              $$ = mycat4($1,pcT,$3,pcX);
              free($1); free($2); free($3); free($4);
*/
         }
         ;

/* parameter entity references from within declaration subset */
pereference: PEREF {
                   printf("AENTNAME NAME %s\n",$1);
                   printf("(PEREF\n)PEREF\n");
                   MsgKwS(msgDEBUG,"Recognizing peref to ",$1,NULL);
              }
              dtdsubset EE {
                   if (($4 != NULL) && (strlen($4) > 0))
                        printf("AENTNAME NAME %s\n",$4);
                   printf("(EE\n)EE\n");
                   MsgKwS(msgDEBUG,"End of peref to ",$1,"(=",$4,")",NULL);
         }
         ;
errorpe  : PEREFERROR {
              printf("AENTNAME NAME %s\n",$1);
              printf("(PEREF\n)PEREF\n");
              printf("<!-- Error opening entity %s -->\n",$1);
              printf("AENTNAME NAME %s\n",$1);
              printf("(EE\n)EE\n");
         }
         ;

/* marked section within declaration subset */

markedsection: mksecstart dtdsubset mksecend
         ;
mksecstart: MDODSO mskeywords DSO {
              printf("AKEYWORD TOKEN %s\n",rgKwms[f]);
              printf("(MSDECL\n%s(MSDATA\n",$2);
              MsgKwS(msgDEBUG,"Marked section (",rgKwms[f],") start",NULL);
              free($2); f = 0;
         }
         ;
mksecend : MSCMDC {
              MsgKwS(msgDEBUG,"Marked section end",NULL);
              printf(")MSDATA\n)MSDECL\n");
         }
         ;
mskeywords: /* nil */ { $$ = mycopy1(""); f = MS_TEMP; }
         | mskeywords mskeyword {
              $$ = mycat2($1,$2); free($1); free($2);
         }
         | mskeywords PEREF mskeywords EE {
              pcT = mycat5($1,"AENTNAME NAME ",$2,
                        "\n(PEREF\n)PEREF\n",$3);
                   if (($4 != NULL) && (strlen($4) > 0)) {
                        $$ = mycat4(pcT,"AENTNAME NAME ",$4,
                                  "\n(EE\n)EE\n");
                   } else {
                        $$ = mycat2(pcT,"(EE\n)EE\n");
                   }
         }
         | mskeywords PEREFERROR {
              pcT = mycat5($1,"AENTNAME NAME ",$2,
                   "\n(PEREF\n)PEREF\n<!-- Error opening entity ",$2);
              $$ = mycat4(pcT," -->\nAENTNAME NAME ",$2,"\n(EE\n)EE\n");
              free($1); free($2); free(pcT);
         }
         ;
mskeyword: KWTEMP    { $$ = PCDoMskw(MS_TEMP);    }
         | KWINCLUDE { $$ = PCDoMskw(MS_INCLUDE); }
         | KWRCDATA  { $$ = PCDoMskw(MS_RCDATA);  }
         | KWCDATA   { $$ = PCDoMskw(MS_CDATA);   }
         | KWIGNORE  { $$ = PCDoMskw(MS_IGNORE);  }
         ;

/********************************************************************/
/* Name Groups                                                      */
/********************************************************************/
namegrp  : seqnames {                            /* cf. 69 */
              $$ = mycat3("ACONN TOKEN SEQ\n(NAMEGRP\n",
                        $1,")NAMEGRP\n");
              free($1);
         }
         | ornames {
              $$ = mycat3("ACONN TOKEN OR\n(NAMEGRP\n",
                        $1,")NAMEGRP\n");
              free($1);
         }
         | andnames {
              $$ = mycat3("ACONN TOKEN AND\n(NAMEGRP\n",
                        $1,")NAMEGRP\n");
              free($1);
         }
         ;
seqnames : namesym {
              $$ = mycat3("(NAME\n-",$1,"\n)NAME\n");
              free($1);
         }
         | seqnames SEQ namesym {
              $$ = mycat4($1,"(NAME\n-",$3,"\n)NAME\n");
              free($1); free($3);
         }
         ;
ornames  : namesym OR namesym {
              $$ = mycat5("(NAME\n-",$1,"\n)NAME\n(NAME\n-",$3,
                          "\n)NAME\n");
              free($1); free($3);
         }
         | ornames OR namesym {
              $$ = mycat4($1,"(NAME\n-",$3,"\n)NAME\n");
              free($1); free($3);
         }
         ;
andnames : namesym AND namesym {
              $$ = mycat5("(NAME\n-",$1,"\n)NAME\n(NAME\n-",
                             $3,"\n)NAME\n");
              free($1); free($3);
         }
         | andnames AND namesym {
               $$ = mycat4($1,"(NAME\n-",$3,"\n)NAME\n");
              free($1); free($3);
         }
         ;
/*
namegrp  : NAME {                              ** cf. 69 **
              printf("ACONN TOKEN SEQ\n");
              printf("(NAMEGRP\n(NAME\n-%s\n)NAME\n)NAMEGRP\n",$1);
         }
         | NAME {
              printf("ACONN TOKEN SEQ\n(NAMEGRP\n");
              printf("(NAME\n-%s\n)NAME\n",$1)
         }
         seqnames {
              printf(")NAMEGRP\n");
         }
         | NAME {
              printf("ACONN TOKEN OR\n(NAMEGRP\n");
              printf("(NAME\n-%s\n)NAME\n",$1);
         }
         ornames {
              printf(")NAMEGRP\n");
         }
         | NAME {
              printf("ACONN TOKEN AND\n(NAMEGRP\n");
              printf("(NAME\n-%s\n)NAME\n",$1);
         }
         andnames {
              printf(")NAMEGRP\n");
         }
         ;
**
namegrp  : GRPO NAME GRPC
         | GRPO NAME seqnames GRPC
         | GRPO NAME ornames GRPC
         | GRPO NAME andnames GRPC
**
seqnames : SEQ NAME {
              printf("(NAME\n-%s\n)NAME\n",$2);
         }
         | seqnames SEQ NAME {
              printf("(NAME\n-%s\n)NAME\n",$3);
         }
         ;
ornames  : OR NAME {
              printf("(NAME\n-%s\n)NAME\n",$2);
         }
         | ornames OR NAME {
              printf("(NAME\n-%s\n)NAME\n",$3);
         }
         ;
andnames : AND NAME {
              printf("(NAME\n-%s\n)NAME\n",$2);
         }
         | andnames AND NAME {
              printf("(NAME\n-%s\n)NAME\n",$3);
         }
         ;
*/

/********************************************************************/
/* Attribute Specification List                                     */
/********************************************************************/
attspeclist : /* nil */ { $$ = mycopy1(""); }    /* cf. 31 */
         | attspeclist attspec {
              $$ = mycat2($1,$2);
              free($1); free($2);
         }
         ;

attspec  : namesym VI value {                       /* cf. 32 */
              $$ = mycat5("AATT NAME ", $1, "\n"
                        "AVALUE CDATA ", $3, "\n"
                        "(DATAATT\n)DATAATT\n");
              free($1); free($3);
         }
         ;
/*       | value */
/* i.e.  | NAME  */
/* name may be omitted only if the attribute has an enumerated range of
** values and the value is an unquoted name token */
/* Need to check P1 to see whether omission of att name is OK */

value    : litstring { $$ = PCFromPLLL($1); }  /* Cf. 33-35 */
         | namesym
         | NUMTOK
         | NUMBER
         ;
/* n.b. a more conventional rewriting of rules 33-35 would produce
** value : literal | name | nametoken | number | number
** but this leads to a reduce-reduce conflict, so we substitute the
** definitions for nametoken and numtoken in, and reduce.
*/
/********************************************************************/
/* Miscellaneous                                                    */
/********************************************************************/
nametoken : namesym
         | NUMBER
         | NUMTOK
         ;
namesym  : NAME
         | KWSYSTEM
         | KWPUBLIC
         | OH
         | KWCDATA
         | KWRCDATA
         | KWEMPTY
         | KWANY
         | KWENTITY
         | KWENTITIES
         | KWID
         | KWIDREF
         | KWIDREFS
         | KWNAME
         | KWNAMES
         | KWNMTOKEN
         | KWNMTOKENS
         | KWNUMBER
         | KWNUMBERS
         | KWNUTOKEN
         | KWNUTOKENS
         | KWNOTATION
         | KWSDATA
         | KWPI
         | KWSTAG
         | KWETAG
         | KWMS
         | KWMD
         | KWNDATA
         | KWSUBDOC
         ;
/* following is unreachable */
numtoken : NUMBER
         | NUMTOK
         ;
pubid    : litstring { $$ = PCFromPLLL($1); }
         ;

%%

/*
#include "dpplex.c"
*/
void DebugRE (struct rEnttext *p) {
    printf("Display of one rEnttext structure:\n");
    pcT = PCFromPLLL(p->plllInt);
    if (p->plllInt != NULL)  {
         printf("  plllInt = \"%s\"\n",pcT);
    }
    if (p->pcPub != NULL)  {
         printf("  pcPub = \"%s\"\n",p->pcPub);
    }
    if (p->pcSys != NULL)  {
         printf("  pcSys = \"%s\"\n",p->pcSys);
    }
    if (p->pcNotn != NULL) {
         printf("  pcNotn = \"%s\"\n",p->pcNotn);
    }
    if (p->pcAtts != NULL) {
         printf("  pcAtts = \"%s\"\n",p->pcAtts);
    }
    printf("i.e.: plllInt=\"%s\"\n"
           "      pcPub=\"%s\"\n"
           "      pcSys=\"%s\"\n"
           "      pcNotn=\"%s\"\n"
           "      pcAtts=\"%s\"\n",
           pcT,p->pcPub,p->pcSys,p->pcNotn,p->pcAtts);
    return;
}
void main(int argc, char *argv[]) {
    int i;
    extern fDebug;
    char c;
    struct rEnttext *pRE;

    /* get arguments and set things up ... */
    while (--argc > 0 && ((*++argv)[0] == '-' || (*argv)[0] == '/'))
/*
    while (--argc > 0 && (*++argv)[0] == '-')
*/
         while (c = *++argv[0])
              switch (c) {
              case 't': fTrace = fDebug = fVerbose = 1;
                        iMsglevel = msgTRACE;
                        yydebug = 1;
                        break;
              case 'd': fDebug = fVerbose = 1;
                        iMsglevel = msgDEBUG;
                        break;
              case 'v': fVerbose = 1;
                        iMsglevel = msgVERBOSE;
                        break;
              case 'I': fMSIgnore = 1;
                        break;
              case 'i': fMSIgnore = 0;
                        break;
              case 'E': fEntwarn = 0;
                        break;
              case 'e': fEntwarn = 1;
                        break;
              default:
                   fprintf(stderr,"dpp:  unknown option %c\n", c);
                   argc = 0;
                   break;
              }
    if (argc > 0) {     /* we have filename args, process them */
         MsgKwS(msgWARNING,"Sorry, filename arguments not implemented yet");
    }
    initfstack();
    lexbegin(CON);
    yyparse();
}
void yyerror(char* s) {
    fprintf(stdout,"L%d %s <-- %s -->\n",cLinecount,fnCurrent,s);
    fprintf(stderr,"! ! ! line %d of %s:  %s ! ! !\n",
              cLinecount,fnCurrent,s);
}
char *myDvKey(char *pKeyword) {
    return mycat3("(DVALUE\nATYPE TOKEN ",pKeyword,
         "\n(DVKEYWD\n)DVKEYWD\n)DVALUE\n");
}
struct rEnttext *myEntLit(enum kwENTTYPES kw, PLLL plll) {
    struct rEnttext *p;

    p = pRENew();
    p->fExt = 0;
    p->kwType = kw;
    p->plllInt = plll;
}
struct rEnttext *pRENew(void) {
    struct rEnttext *p;

/*  p = (struct rEnttext *) malloc (sizeof(struct rEnttext)); */
    p = (struct rEnttext *) malloc (sizeof(*p));
    if (p == NULL) {
         yyerror("No memory for external ID\n");
         exit(8);
    } else {
         p->fExt = -1;
         p->plllInt = NULL;
         p->pcPub = NULL;
         p->pcSys = NULL;
         p->kwType = kwNILTYPE;
         p->pcNotn = NULL;
         p->pcAtts = NULL;
    }
    return p;
}

void FreemembersRE (struct rEnttext *p) {
    if (p->plllInt != NULL)  { FreePLLL(p->plllInt); };
    if (p->pcPub != NULL)  { free(p->pcPub); };
    if (p->pcSys != NULL)  { free(p->pcSys); };
    if (p->pcNotn != NULL) { free(p->pcNotn); };
    if (p->pcAtts != NULL) { free(p->pcAtts); };
    return;
}

char * PCExtid (struct rEnttext *p) {
    if ((p->pcSys == NULL) && (p->pcPub == NULL)) {
         return mycopy1("(EXTID\n(SYSTEM\n)SYSTEM\n)EXTID\n");
    } else if ((p->pcSys == NULL) && (p->pcPub != NULL)) {
         return mycat3("(EXTID\n(PUBLIC\n-",p->pcPub,
                        "\n)PUBLIC\n)EXTID\n");
    } else if ((p->pcSys != NULL) && (p->pcPub == NULL)) {
         return mycat3("(EXTID\n(SYSTEM\n)SYSTEM\n(LITERAL\n-",
                        p->pcSys,"\n)LITERAL\n)EXTID\n");
    } else if ((p->pcSys != NULL) && (p->pcPub != NULL)) {
         return mycat5("(EXTID\n(PUBLIC\n-",p->pcPub,
                        "\n)PUBLIC\n(LITERAL\n-",p->pcSys,
                        "\n)LITERAL\n)EXTID\n");
    }
};

static char *rgENTTYPES[] = { "NIL TYPE",
                  "CDATA", "SDATA", "PI",
                  "STARTTAG", "ENDTAG", "MS", "MD",
                  "SUBDOC", "NDATA",
                  "NORMAL"
};

char * PCEnttype (struct rEnttext *p) {
    char * pcX;

    if (p->kwType == kwNILTYPE) {
         return mycopy1("");
    } else if (p->kwType == kwNORMAL) {
         return mycopy1("");
    } else if (p->kwType == kwSUBDOC) {
         return mycopy1("(EXTTYPE\n(SUBDOC\n)SUBDOC\n)EXTTYPE\n");
    } else if (p->pcAtts == NULL) {
         return mycat5("(EXTTYPE\nATYPE TOKEN ",
                        rgENTTYPES[p->kwType],
                        "\nANOTATION NAME ",
                        p->pcNotn,
                        "\n(EXTDATA\n)EXTDATA\n)EXTTYPE\n");
    } else { /* p->pcAtts != NULL */
         pcX = mycat4("ATYPE TOKEN ",rgENTTYPES[p->kwType],
                        "\nANOTATION NAME ",p->pcNotn);
         return mycat5("(EXTTYPE\n",pcX,
                        "\n(EXTDATA\n",p->pcAtts,
                        ")EXTDATA\n)EXTTYPE\n");
    }
};

void PrintFileEnttext(FILE * pfile, struct rEnttext *p) {
    char * pcT;
    char * pcX;

    if (p->fExt) {                     /* external entity */
         pcT = PCExtid(p);
         pcX = PCEnttype(p);
         fprintf(pfile,"(EXTERNAL\n%s%s)EXTERNAL\n",pcT,pcX);
    } else {                           /* internal entity */
         fprintf(pfile,"ATYPE TOKEN %s\n(LITERAL\n",
                   rgENTTYPES[p->kwType]);
         PrintFileLLL(pfile,p->plllInt);
         fprintf(pfile,")LITERAL\n");
    }
}
char * PCDoMskw(enum kwMSTYPES kw) {
    char * pc;
    pc = mycat3("AKEYWORD TOKEN ",rgKwms[kw],"\n(MSKEY\n)MSKEY\n");
    f  = (f > kw ? f : kw);
    return pc;
}
