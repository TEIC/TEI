/* carthago.y:  DTD pre-processor.  Reads DTD, emits DTD, omitting
references to some elements (Carthago delenda est!) */
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
** 23 Jun 99 make XML flags affect attribute declarations
** 29 Apr 99 add flag --xml to cause suppression of omissibility info
**           add flag --xmlpe to cause emission of parameter entities ...
**           add flags --wrap and --nowrap to control line wrapping
** 2 Dec 97  Change definition of NEEDSWRAP
** 13-14 Oct 97 move treatment of dead tokens from grammar into actions.
**           The grammatical treatment is too error prone because
**           too hard to understand.
** 30 Apr 96 split modelgrp into multigrp and unarygrp, to fix bug
**           introduced by clever way of stripping excess parens
** 26 Apr 96 run timing test:  old dpp | tf.tcl script takes
**           80 Mb of RAM, 22 minutes of clock time, 19 minutes CPU
**           carthago takes 500 Kb RAM, 2 runs of 3 sec. each
**
**           rationalize set of options
**           change grammar to gain special rule, to remove
**           parens from groups with only one member
**           allow distinct read and write files of delenda
** 25 Apr 96 add hash tables for declared, undeclared elements
**           discover nasty memory error, spend time with gdb
**           find and remove erroneous calls to free(), add
**           donotfree() as reminder for certain items
** 24 Apr 96 improve argument parsing (use strcmp)
** 23 Apr 96 make carthago.y from dpp.y:  augment grammar to handle
**           deleted gis, test for ambiguity
**  1 Sep 95 initialize fnCurrent to "<stdin>" (as part of moving msg.c
**           into a library)
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
- This parser allows exceptions of the form -(a,b)+(c,d), which I
  now believe is an error.  Check the standard!
*/
/* Known non-bug shortcomings:
- ps is silently eaten by lexical scanner, instead of being presented
  to output stream
- This version of the parser really ought to be integrated into dpp,
  not kept separate!
*/

%{
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define myfree(X) free(X)
#define donotfree(X) {}
/*
#define myfree(X) {}
*/

#define OCC(X) X[0]
#define STATUS(X) X[1]
/*
  $$[0] = the occurrence indicator (1, ?, +, *)
  $$[1] = the child-count (0, 1, 2 or more, 'e' for errors)
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <assert.h>

#define bcopy(A,B,C) memcpy((B),(A),(C))
/* #define alloca(A)    malloc(A) */
#define max(A,B)  A > B ? A : B
void yyerror(char* s);
char occurs(char cOcc1, char cOcc2);
char status(char cStat, char cOcc);
char * SClotheE(char * pcExpression);
char * EClotheE(char * pcExpression);
char * EBuildseqEEK(char * pc1, char * pc3, char cConn);
char * EBuildaltEE(char * pc1, char * pc3);
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

char *pcT;
char *pcX;
char *pcT1;
char *pcT2;
char *pcT3;
char *pcT4;
char * sCurrentGI;
int  i, j, k, iLen;
int f;
#define cLinelen 55

#include "mycat.h"
#include "lllit.h"
#include "entmgr.h"
#include "dpplex.h"
#include "msg.h"
#include "hash.h"
int fEntwarn = 0;
int fKeepPEs = 0;
int fKeepComms = 1;
int fLinewrap = 1;
int fXML = 0;
int fXMLpe = 0;
int fOmpedone = 0;
HASHTABLE htDeclared;
HASHTABLE htReferenced;
HASHTABLE htDelenda;

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
**   'Token types' and DELETED
*/
/* Delimiters, including the anomalous PERO + SPACE */
%token MDC GRPO GRPC DSO DSC COM PIO PIC VI
%token PEROSPACE LITDELIM
%token MDODSO MSCMDC

/* Compound keywords */
%token MDELEMENT MDDOCTYPE MDOCOM MDOMDC MDATTLIST
%token MDENTITY  MDNOTATION

/* Operators for minimization, content models, and exceptions */
%left  AND OR SEQ
%token OPT PLUS REP MINUS OH
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
/* Deleted:  names of deleted elements */
%token DELETED CARTHAGO EST

%type <s> NAME STRING LITERAL NUMBER NUMTOK
%type <s> KWCDATA KWRCDATA KWEMPTY KWANY
%type <s> KWSYSTEM KWPUBLIC OH
%type <s> KWENTITY KWENTITIES KWID KWIDREF KWIDREFS
%type <s> KWNAME KWNAMES KWNMTOKEN KWNMTOKENS KWNUMBER KWNUMBERS
%type <s> KWNUTOKEN KWNUTOKENS KWNOTATION
%type <s> KWSDATA KWPI KWSTAG KWETAG KWMS KWMD KWNDATA KWSUBDOC
%type <s> PEREF EE PEREFERROR
%type <s> DELETED
%type <s> delenda
%type <s> elementdecl commdecl commseq procinst attlistdecl
%type <s> elemtype contentdecl namegrp andnames ornames seqnames
%type <s> modelgrp
%type <s> model tokengrp andtokens ortokens seqtokens token
%type <s> goodexceptiongrp badexceptiongrp
%type <s> minimiz min
%type <s> exceptions inclusions exclusions
%type <s> associated attdeflist attdef valtype default value
%type <s> assocnotatn
%type <s> nmtokgrp nmtokseq nmtokcom nmtokbar nmtokamp nametoken
%type <s> attspeclist attspec
%type <s> sysid pubid namesym numtoken
%type <s> realname
%type <s> markedsection mksecstart mskeywords mskeyword
%type <e> extid externdtd enttext enttype
%type <v> enttypekw
%type <lllit> litstring litdata

%%
cde      : prolog
         | CARTHAGO delenda EST
         ;
delenda  : /* */ { $$ = mycopy1(""); }
         | delenda DELETED {
             MsgKwS(msgVERBOSE,$2," memoranda est.",NULL);
             FInstallKeyHtPRec($2,htDelenda,NULL);
             MsgKwS(msgVERBOSE,$2," delenda est.",NULL);
         }
         ;

prolog   : misc dtdseq                           /* cf. 7, 9 */
         | misc nonmisc dtdsubset
         | misc
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
              printf("<!--* carthage found fatal error, aborting ... *-->\n");
              printf("<!--* Are you sure the URL you gave is correct? *-->\n");
         }
         ;

dtdseq   : dtd misc                              /* cf. 7 */
         | dtdseq dtd misc
         ;
dtd      : MDDOCTYPE dtdname externdtd dso dtdsubset dsc exmdc
           /* cf. 110 */
         | MDDOCTYPE dtdname           dso dtdsubset dsc MDC
         | MDDOCTYPE dtdname externdtd                   exmdc
         | MDDOCTYPE dtdname                             MDC {
              /* code for external DTD file here ... */
              /* none yet.  In the meantime ... */
              MsgKwS(msgWARNING,"Vacuous DTD:  no external DTD entity, "
                             "no DTD subset",NULL);
         }
         ;
dtdname  : realname /* cf. 111 */ {
             MsgKwS(msgVERBOSE,"Reading DTD for ",$1,NULL);
         }
         | DELETED {
             MsgKwS(msgERROR,"Document element ",$1,
                    " marked for deletion",NULL);
         }
         ;
externdtd: extid { /* external identifier for DTD */
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
              myfree($1);
         }
         ;
dso      : DSO     /* opening of doc type decl subset */
         ;
dsc      : DSC     /* closing of doc type decl subset */
         ;
exmdc    : MDC {   /* end of doc type decl with external DTD file */
                   /* code for external DTD file here ... */
                   if (FOpenEntity("#extDTD")) {
                        lexbegin(DS);
                   } else { MsgKwS(msgERROR,
                             "Could not open external DTD file",NULL);
                   }
              }
           dtdsubset EE
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
              printf("<!--* carthage found fatal error, aborting ... *-->\n");
              printf("<!--* Are you sure the URL you gave is correct? *-->\n");
         }
         ;

/********************************************************************/
/* Element Declarations                                             */
/********************************************************************/
elementdecl : MDELEMENT elemtype minimiz contentdecl MDC { /* cf. 116 */
/*            printf("(ELEMENT\n%s%s%s)ELEMENT\n",$2,$3,$4);
*/
              if ((!fOmpedone) && fXMLpe) {
                fOmpedone = 1;
		printf("\n<!ENTITY %% om.RR '- -'>\n");
		printf(  "<!ENTITY %% om.RO '- O'>\n");
		printf(  "<!ENTITY %% om.OR 'O -'>\n");
		printf(  "<!ENTITY %% om.OO 'O O'>\n\n");
	      }
              if (fLinewrap) {
		printf("\n<!ELEMENT %s %s\n",$2,$3);
		{ 
		  char * sFull = $4;
		  char * sT;
		  while ((strlen(sFull) > cLinelen) 
			 && ((sT = strpbrk(sFull+cLinelen," ")) != NULL)) {
		    /* there are blanks in what is left */
		    *sT = '\0';
		    printf("\t%s \n",sFull);
		    sFull = sT + 1;
		  }
		  printf("\t%s >\n\n",sFull);
		}
	      } else {
		/* no linewrap! */
		printf("\n<!ELEMENT %s %s %s >\n\n",$2,$3,$4);
	      }
              {
                char * pcNm;
                char * pcElementtype;
                int fTemp;

                pcElementtype = mycopy1($2);
                pcNm = strtok(pcElementtype," |,&()");
                while (pcNm != NULL) {
                  if (fDebug) {
                    MsgKwS(msgDEBUG,"Declaring ",pcNm,NULL);
                    /* fprintf(stderr,"Declaring %s\n",pcNm); */
                  };
                  fTemp = FInstallKeyHtPRec(pcNm,htDeclared,NULL);
                  if (PPRLookupKeyHt(pcNm,htDelenda) != NULL) {
                    MsgKwS(msgERROR,"Element ",pcNm," is marked ",
                           "for deletion, but is declared.",NULL);
                  };
                  pcNm = strtok(NULL," |,&()");
                };
              }
              myfree($2); myfree($3); myfree($4);
         }
         ;

elemtype : namesym {                            /* cf. 117 */
              $$ = $1;
              sCurrentGI = mycopy1($1);
         }
         | GRPO namegrp GRPC {
              $$ = mycat3("(",$2,")");
              sCurrentGI = mycat3("(",$2,")");
              myfree($2);
         }
         ;

minimiz  : /* nil */ {                           /* cf. 122-24 */
              $$ = mycopy1("");
         }
         | min min {                             /* cf. 122-24 */
	      if (fXML) {
		$$ = mycopy1("");
	      } else if (fXMLpe) {
		if ((strcmp($1,"-")==0)&&(strcmp($2,"-")==0)) {
		  $$ = mycopy1("%om.RR;");
		} else if ((strcmp($1,"-")==0)&&(strcmp($2,"O")==0)) {
		  $$ = mycopy1("%om.RO;");
		} else if ((strcmp($1,"O")==0)&&(strcmp($2,"-")==0)) {
		  $$ = mycopy1("%om.OR;");
		} else if ((strcmp($1,"O")==0)&&(strcmp($2,"O")==0)) {
		  $$ = mycopy1("%om.OO;");
		}
	      } else {
		$$ = mycat4($1," ",$2," ");
		donotfree($1); donotfree($2);
	      }
         }
         ;
min      : MINUS                { $$ = "-"; }
         | OH                   { $$ = "O"; }
         ;


contentdecl : KWCDATA  { 
              if (fXML || fXMLpe) {
                 $$ = mycopy1("(#PCDATA)");
              } else {
                 $$ = mycopy1("CDATA");  /* 125f */
              }
         }
         | KWRCDATA    { 
              if (fXML || fXMLpe) {
                 $$ = mycopy1("(#PCDATA)"); 
	      } else {
                 $$ = mycopy1("RCDATA"); 
	      }
         }
         | KWEMPTY     { $$ = mycopy1("EMPTY"); }
         | KWANY exceptions    {
              $$ = mycat2("ANY",$2);
              myfree($2);
         }
         | model exceptions    {
              $$ = mycat2($1,$2);
              myfree($1); myfree($2);
         }
         ;

/* model groups:
This section of the grammar recognizes model groups, deleting GIs
along the way whenever the user has specified that they are to be
deleted, and they can in fact be deleted without error.
The return value is a string, with a two-character prefix showing
two attributes:
  $$[0] = the occurrence indicator (1, ?, +, *)
  $$[1] = the status / child count (0, 1, 2 or more, 'e' for errors)
*/

model    : modelgrp {
              /* if modelgrp is empty, then signal
                 MsgKwS(msgERROR,"Model has no content left:  ",$1,NULL);
              else if modelgrp is an error group, then signal
                 MsgKwS(msgERROR,"Model requires hand work:  ",$1,NULL);
              */
              if (STATUS($1)=='0') {
                 MsgKwS(msgERROR,"Model has no content left:  ",$1+2,NULL);
              } else if (STATUS($1)=='e') {
                 MsgKwS(msgERROR,"Model requires hand work:  ",$1+2,NULL);
              }
              if (STATUS($1)=='1') {
		STATUS($1) = ',';
	      };
              $$ = SClotheE($1);
              free($1);
         }
         ;

/* content model group nests, unlike other groups.  Cf. 127 */
modelgrp : GRPO tokengrp GRPC {
              $$ = $2;
         }
         | GRPO tokengrp GRPOPT {
              $$ = $2;
              OCC($$) = occurs(OCC($2),'?');
              STATUS($$) = status(STATUS($2),OCC($$));
         }
         | GRPO tokengrp GRPREP {
              $$ = $2;
              OCC($$) = occurs(OCC($2),'*');
              STATUS($$) = status(STATUS($2),OCC($$));
         }
         | GRPO tokengrp GRPPLUS {
              $$ = $2;
              OCC($$) = occurs(OCC($2),'+');
              STATUS($$) = status(STATUS($2),OCC($$));
         }
         ;


tokengrp : seqtokens                             /* cf. 127 */
         | ortokens
         | andtokens
         ;
seqtokens : token
         | seqtokens SEQ token {
              $$ = EBuildseqEEK($1,$3,',');
         }
         ;
ortokens : token OR token {
              $$ = EBuildaltEE($1,$3);
         }
         | ortokens OR token {
              $$ = EBuildaltEE($1,$3);
         }
         ;
andtokens : token AND token {
              $$ = EBuildseqEEK($1,$3,'&');
         }
         | andtokens AND token {
              $$ = EBuildseqEEK($1,$3,'&');
         }
         ;

token    : RNIPCDATA {
              $$ = mycopy1("11#PCDATA");
         }
         | realname {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("11",$1);
         }
         | realname OPT {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("?1",$1);
         }
         | realname REP {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("*1",$1);
         }
         | realname PLUS {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("+1",$1);
         }
         | DELETED {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("1eZOMBIE...",$1); free($1);
         }
         | DELETED OPT {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("?0ZOMBIE...",$1); free($1);
         }
         | DELETED REP {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("*0ZOMBIE...",$1); free($1);
         }
         | DELETED PLUS {
              FInstallKeyHtPRec($1,htReferenced,NULL);
              $$ = mycat2("1eZOMBIE...",$1); free($1);
         }
         | modelgrp { $$ = $1; }
         ;

exceptions : exclusions inclusions {             /* cf. 138-40 */
              if ((strlen($1) + strlen($2)) > 0) {
                $$ = mycat4(" ",$1," ",$2);
                myfree($1); myfree($2);
              } else {
                $$ = mycopy1("");
              }
         }
         ;

/* lexx misreads -(a,b)+(c,d) so we need special case */
/* no, we don't, -(a,b)+(c,d) is illegal by rule 138  */
/*
         | MINUSGRP namegrp GRPPLUS GRPO namegrp GRPC {
              $$ = mycat5(" -(",$2,") +(",$5,")");

         }
         ;
*/
exclusions : /* empty */          { $$ = mycopy1(""); }
         | MINUSGRP goodexceptiongrp GRPC  {
              $$ = mycat3("-(",$2,")");
              myfree($2);
         }
         | MINUSGRP badexceptiongrp GRPC  {
              $$ = mycopy1(""); myfree($2);
         }
         ;
inclusions : /* empty */          { $$ = mycopy1(""); }
         | PLUSGRP goodexceptiongrp GRPC   {
              $$ = mycat3("+(",$2,")");
              myfree($2);
         }
         | PLUSGRP badexceptiongrp GRPC   {
              $$ = mycopy1(""); myfree($2);
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
              printf("\n<!ATTLIST %s %s >\n\n",
                   $2,$3);
              myfree($2); myfree($3);
         }
         ;
associated : elemtype
         | assocnotatn
         ;

attdeflist : attdef                         /* Cf. 142 */
         | attdef attdeflist {
              $$ = mycat2($1,$2); myfree($1); myfree($2);
         }
         ;

attdef   : namesym valtype default {           /* Cf. 143-44 */
              pcT = mycat4("\n\t",$1," ",$2);
              $$ = mycat3(pcT," ",$3);
              myfree($1); myfree($2); myfree($3); myfree(pcT);
         }
         ;

valtype  : KWCDATA      { $$ = mycopy1("CDATA");}/* cf. 145 */
         | KWENTITY     { $$ = mycopy1("ENTITY");   }
         | KWENTITIES   { $$ = mycopy1("ENTITIES"); }
         | KWID         { $$ = mycopy1("ID");       }
         | KWIDREF      { $$ = mycopy1("IDREF");    }
         | KWIDREFS     { $$ = mycopy1("IDREFS");   }
         | KWNAME       { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKEN");
	   } else {
	     $$ = mycopy1("NAME");     
	   }
         }
         | KWNAMES      { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKENS");
	   } else {
	     $$ = mycopy1("NAMES");    
	   }
         }
         | KWNMTOKEN    { $$ = mycopy1("NMTOKEN");  }
         | KWNMTOKENS   { $$ = mycopy1("NMTOKENS"); }
         | KWNUMBER     { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKEN");
	   } else {
	     $$ = mycopy1("NUMBER");   
	   }
         }
         | KWNUMBERS    { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKENS");
	   } else {
	     $$ = mycopy1("NUMBERS");  
	   }
         }
         | KWNUTOKEN    { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKEN");
	   } else {
	     $$ = mycopy1("NUTOKEN");  
	   }
         }
         | KWNUTOKENS   { 
	   if (fXML || fXMLpe) {
	     $$ = mycopy1("NMTOKENS");
	   } else {
	     $$ = mycopy1("NUTOKENS"); 
	   }
         }
         | KWNOTATION GRPO namegrp GRPC {
              $$ = mycat3("NOTATION (",$3,")");
              myfree($3);
         }
         | nmtokgrp     {
              $$ = $1;
              donotfree($1);
         }
         ;

nmtokgrp : GRPO nmtokseq GRPC { $$ = mycat3("(",$2,")"); } /* 68, 131 */
         ;
nmtokseq : nmtokcom {
              $$ = $1;
         }
         | nmtokbar {
              $$ = $1;
         }
         | nmtokamp {
              $$ = $1;
         }
         ;
nmtokcom : nametoken {
              $$ = $1;
         }
         | nmtokcom SEQ nametoken {
              $$ = mycat3($1,", ",$3);
              myfree($1); myfree($3);
         }
         ;
nmtokbar : nametoken OR nametoken {
              $$ = mycat3($1," | ",$3);
              myfree($1); myfree($3);
         }
         | nmtokbar  OR nametoken {
              $$ = mycat3($1," | ",$3);
              myfree($1); myfree($3);
         }
         ;
nmtokamp : nametoken AND nametoken {
              $$ = mycat3($1," & ",$3);
              myfree($1); myfree($3);
         }
         | nmtokamp  AND nametoken {
              $$ = mycat3($1," & ",$3);
              myfree($1); myfree($3);
         }
         ;

default  :  value {                          /* Cf. 147 */
              $$ = $1;
         }
         | RNIFIXED value {
              $$ = mycat2("#FIXED ",$2);
              myfree($2);
         }
         | RNIREQUIRED {
              $$ = mycopy1("#REQUIRED");
         }
         | RNICURRENT {
              if (fXML || fXMLpe) {
                 $$ = mycopy1("#IMPLIED");
              } else {
                 $$ = mycopy1("#CURRENT");
              }
         }
         | RNICONREF {
              if (fXML || fXMLpe) {
                 $$ = mycopy1("#IMPLIED");
	      } else {
                 $$ = mycopy1("#CONREF");
	      }
         }
         | RNIIMPLIED {
              $$ = mycopy1("#IMPLIED");
         }
         ;

assocnotatn : RNINOTATION namesym {                 /* Cf. 149.1 */
              $$ = mycat2("#NOTATION ",$2);
              myfree($2);
         }
         | RNINOTATION GRPO namegrp GRPC {
              $$ = mycat3("#NOTATION (",$3,")");
              myfree($3);
         }
         ;

/********************************************************************/
/* Notation Declarations                                            */
/********************************************************************/
notndecl : MDNOTATION namesym extid MDC {
              pcT = PCExtid($3);
              printf("<!NOTATION %s %s >\n",$2,pcT);
              FreemembersRE($3); myfree(pcT);
              myfree($2); myfree($3);
         }
         ;

/********************************************************************/
/* Entity Declarations                                              */
/********************************************************************/
/* entity set */
/* need to replace enttext string with call to myentlit, or ... */
entitydecl : MDENTITY  namesym enttext MDC {   /* cf. 101-104 */
              printf("\n<!ENTITY %s ",$2);
              PrintFileEnttext(stdout,$3);
              printf(" >\n\n");
              FreemembersRE($3);
              myfree($2); myfree($3);
         }
         | MDENTITY  RNIDEFAULT enttext MDC {
              printf("\n<!ENTITY #DEFAULT ");
              PrintFileEnttext(stdout,$3);
              printf(" >\n\n");
              FreemembersRE($3);
              myfree($3);
         }
         | MDENTITY  PEROSPACE  namesym enttext MDC {
              if (fKeepPEs) {
                printf("\n<!ENTITY %% %s ",$3);
                PrintFileEnttext(stdout,$4);
                printf(" >\n\n");
              };
              MsgKwS(msgDEBUG,"<!ENTITY % ",$3," \"...\">",NULL);
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
                   MsgKwS(msgTRACE,"Entity %% ",$3," has been installed",
                          NULL);
              }
              FreemembersRE($4); myfree(pcT1);
              myfree($3); myfree($4);
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
              myfree($1);
              donotfree($2);
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

enttypekw: KWCDATA { $$ = kwCDATA; myfree($1); }
         | KWNDATA { $$ = kwNDATA; myfree($1); }
         | KWSDATA { $$ = kwSDATA; myfree($1); }
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
              if (fKeepComms) {
                printf("<!--%s--%s>\n",$2,$4);
              };
              myfree($2); myfree($4);
         }
         | MDOMDC {
              if (fKeepComms) {
                printf("\n<!>\n\n");
              }
         }
         ;
commseq  : /* nothing */ {
              $$ = mycopy1("");
         }
         | commseq COM STRING COM {
              $$ = mycat4($1,"\n--",$3,"--");
              myfree($1); myfree($3);
         }
         ;
procinst : PIO STRING PIC {                      /* cf. 44 */
              printf("<?%s>\n",$2);
              myfree($2);
         }
         ;

litstring : LITDELIM litdata LITDELIM { $$ = $2;
              if (fDebug) {
                   pcT = PCFromPLLL($2);
                   MsgKwS(msgTRACE,"Literal string recognized:  ",pcT,NULL);
              }
         }
         ;
litdata : /* nil */   { $$ = LLLCreate(); }
         | litdata LITERAL {
              $$ = LLLAppend($1,LLL_LITERAL_NODE,$2);
/*            pcT = myescape($2);
              $$ = mycat2($1,pcT);
              myfree($1); myfree($2); myfree(pcT);
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
              myfree($1); myfree($2); myfree($3); myfree($4);
*/
         }
         ;

/* parameter entity references from within declaration subset */
pereference: PEREF {
                   MsgKwS(msgTRACE,"Recognizing peref to ",$1,NULL);
              }
              dtdsubset EE {
                   MsgKwS(msgTRACE,"End of peref to ",$1,"(=",$4,")",NULL);
         }
         ;
errorpe  : PEREFERROR {
              printf("<!-- Error opening entity %s -->\n",$1);
         }
         ;

/* marked section within declaration subset */

markedsection: mksecstart dtdsubset mksecend
         ;
mksecstart: MDODSO mskeywords DSO {
              if (fVerbose) printf("\n<![ %s [",rgKwms[f]);
              MsgKwS(msgTRACE,"Marked section (",rgKwms[f],") start",NULL);
              myfree($2); f = 0;
         }
         ;
mksecend : MSCMDC {
              MsgKwS(msgTRACE,"Marked section end",NULL);
              if (fVerbose) printf("]]>\n\n");
         }
         ;
mskeywords: /* nil */ { $$ = mycopy1(""); f = MS_TEMP; }
         | mskeywords mskeyword {
              $$ = mycat2($1,$2); myfree($1); myfree($2);
         }
         | mskeywords PEREF mskeywords EE {
              $$ = mycat5($1,"%",$2,";",$3);
         }
         | mskeywords PEREFERROR {
              pcT = mycat5($1,"%",$2,
                   "; <!-- Error opening entity ",$2);
              $$ = mycat2(pcT," -->\n");
              myfree($1); myfree($2); myfree(pcT);
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
/*
exceptiongrp : goodexceptiongrp
         | badexceptiongrp
         ;
*/
goodexceptiongrp : realname               {
                      $$ = $1;
                      FInstallKeyHtPRec($1,htReferenced,NULL);
         }
         | badexceptiongrp conn realname  {
              $$ = $3;
              FInstallKeyHtPRec($3,htReferenced,NULL);
              myfree($1);
         }
         | goodexceptiongrp conn realname {
              $$ = mycat3($1," | ",$3);
              FInstallKeyHtPRec($3,htReferenced,NULL);
              myfree($1);
              myfree($3);
         }
         | goodexceptiongrp conn DELETED  {
              $$ = $1;
              FInstallKeyHtPRec($3,htReferenced,NULL);
              myfree($3);  /* donotfree($2); */
         }
         ;
badexceptiongrp : DELETED                 {
                     $$ = mycopy1("");
                     FInstallKeyHtPRec($1,htReferenced,NULL);
         }
         | badexceptiongrp conn DELETED   {
              $$ = $1;
              FInstallKeyHtPRec($3,htReferenced,NULL);
              myfree($3);
         }
         ;
conn     : AND
         | OR
         | SEQ
         ;

namegrp  : seqnames {                            /* cf. 69 */
              $$ = $1;
         }
         | ornames {
              $$ = $1;
         }
         | andnames {
              $$ = $1;
         }
         ;
seqnames : namesym {
              $$ = $1;
         }
         | seqnames SEQ namesym {
              $$ = mycat3($1,", ",$3);
              myfree($1); myfree($3);
         }
         ;
ornames  : namesym OR namesym {
              $$ = mycat3($1," | ",$3);
              myfree($1); myfree($3);
         }
         | ornames OR namesym {
              $$ = mycat3($1," | ",$3);
              myfree($1); myfree($3);
         }
         ;
andnames : namesym AND namesym {
              $$ = mycat3($1," & ",$3);
              myfree($1); myfree($3);
         }
         | andnames AND namesym {
              $$ = mycat3($1," & ",$3);
              myfree($1); myfree($3);
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
              myfree($1); myfree($2);
         }
         ;

attspec  : namesym VI value {                       /* cf. 32 */
              $$ = mycat3($1," = ",$3);
              myfree($1); myfree($3);
         }
         ;
/*       | value */
/* i.e.  | NAME  */
/* name may be omitted only if the attribute has an enumerated range of
** values and the value is an unquoted name token */
/* Need to check P1 to see whether omission of att name is OK */

value    : litstring {
             { char * pcT; char * qT;
             pcT = PCFromPLLL($1);
             if (strchr(pcT,'\"') == NULL) {
               qT = "\"";
             } else {
               qT = "\'";
             }
             $$ = mycat3(qT,pcT,qT);
             }
         }  /* Cf. 33-35 */
         | namesym { $$ = mycat3("\"",$1,"\""); }
         | NUMTOK  { $$ = mycat3("\"",$1,"\""); }
         | NUMBER  { $$ = mycat3("\"",$1,"\""); }
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
namesym  : realname
         | DELETED
         ;
realname : NAME
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
};

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
    int fDeclared = 0, 
        fUsed = 0, 
        fUndeclared = 1, 
        fUnused = 0;

    htDelenda    = HtInit();
    htDeclared   = HtInit();
    htReferenced = HtInit();
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
        fprintf(stderr,"  --wrap* --nowrap"
                " (wrap lines in element declarations)\n");
        fprintf(stderr,"  -x --xml --xmlpe -S --sgml*"
                " (Use SGML tag omissibility marks, or none, or entities)\n");
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
      } else if ((strcmp(argv[argCur],"-x"   ) == 0) ||
                 (strcmp(argv[argCur],"--xml") == 0)) {
        fXML = 1; fXMLpe = 0;
      } else if (strcmp(argv[argCur],"--xmlpe") == 0) {
        fXML = 0; fXMLpe = 1;
      } else if ((strcmp(argv[argCur],"-S"    ) == 0) ||
                 (strcmp(argv[argCur],"--sgml") == 0)) {
        fXML = 0; fXMLpe = 0;
      } else if (strcmp(argv[argCur],"--wrap") == 0) {
        fLinewrap = 1;
      } else if (strcmp(argv[argCur],"--nowrap") == 0) {
        fLinewrap = 0;
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


    if (fnRptfile != NULL) {
      { FILE * pfRptfile = fopen(fnRptfile,"w");
        HASHTABLE htTemp;
        HTLINKEDLIST htl;
        phtrec pTemp;

        if (fDeclared) {
          /* report on all items declared */
          htl = HtllFromHt(htDeclared);
          fprintf(pfRptfile,"<declared>\n");
          for (pTemp = (phtrec) htl; pTemp != NULL; pTemp = (phtrec) htl) {
            fprintf(pfRptfile,"%s\n",pTemp->pcKey);
            htl = (HTLINKEDLIST) pTemp->prNext;
            myfree(pTemp);
          };
          fprintf(pfRptfile,"</declared>\n");
        }
        if (fUsed) {
          /* report on all items used */
          htl = HtllFromHt(htReferenced);
          fprintf(pfRptfile,"<used>\n");
          for (pTemp = (phtrec) htl; pTemp != NULL; pTemp = (phtrec) htl) {
            fprintf(pfRptfile,"%s\n",pTemp->pcKey);
            htl = (HTLINKEDLIST) pTemp->prNext;
            myfree(pTemp);
          };
          fprintf(pfRptfile,"</used>\n");
        }
        if (fUnused) {
          /* report on items declared but not referenced */
          htTemp = HtDiffHtHt(htDeclared,htReferenced);
          htl = HtllFromHt(htTemp);
          fprintf(pfRptfile,"<unused>\n");
          for (pTemp = (phtrec) htl; pTemp != NULL; pTemp = (phtrec) htl) {
            MsgKwS(msgVERBOSE,pTemp->pcKey," declared but not used.",NULL);
            fprintf(pfRptfile,"%s\n",pTemp->pcKey);
            htl = (HTLINKEDLIST) pTemp->prNext;
            myfree(pTemp);
          };
          fprintf(pfRptfile,"</unused>\n");
        }
        if (fUndeclared) {
          /* report on items referenced but not declared */
          htTemp = HtDiffHtHt(htReferenced,htDeclared);
          htl = HtllFromHt(htTemp);
          fprintf(pfRptfile,"<undeclared>\n");
          fprintf(pfRptfile,"<!-- haec delenda sunt -->\n");
          for (pTemp = (phtrec) htl; pTemp != NULL; pTemp = (phtrec) htl) {
            MsgKwS(msgVERBOSE,pTemp->pcKey," used but not declared.",NULL);
            fprintf(pfRptfile,"%s\n",pTemp->pcKey);
            htl = (HTLINKEDLIST) pTemp->prNext;
            myfree(pTemp);
          };
          fprintf(pfRptfile,"</undeclared>\n");
        }
        fclose(pfRptfile);
      }; /* compound statement with local vars */
    }; /* if report file was specified */
}
void yyerror(char* s) {
    fprintf(stdout,"L%d %s <-- %s -->\n",cLinecount,fnCurrent,s);
    /* fprintf(stderr,"! ! ! line %d of %s:  %s ! ! !\n", 
              cLinecount,fnCurrent,s); */
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
    return p;
};

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
};

void FreemembersRE (struct rEnttext *p) {
    if (p->plllInt != NULL)  { FreePLLL(p->plllInt); };
    if (p->pcPub != NULL)  { myfree(p->pcPub); };
    if (p->pcSys != NULL)  { myfree(p->pcSys); };
    if (p->pcNotn != NULL) { myfree(p->pcNotn); };
    if (p->pcAtts != NULL) { myfree(p->pcAtts); };
    return;
};

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
         return mycopy1("SUBDOC");
    } else if ((p->pcAtts == NULL) || (p->pcNotn == NULL)) {
         return mycopy1(rgENTTYPES[p->kwType]);
    } else if ((p->pcAtts == NULL) || (p->pcNotn != NULL)) {
         return mycat3(rgENTTYPES[p->kwType]," ",p->pcNotn);
    } else { /* p->pcAtts != NULL */
         pcX = mycat3(rgENTTYPES[p->kwType]," ",p->pcNotn);
         return mycat4(pcX," [",p->pcAtts,"]");
    }
};

void PrintFileEnttext(FILE * pfile, struct rEnttext *p) {
  char * pcT;
  char * pcX;

  if (p->fExt) {                     /* external entity */
    pcT = PCExtid(p);
    pcX = PCEnttype(p);
    fprintf(pfile,"%s %s",pcT,pcX);
  } else {                           /* internal entity */
    fprintf(pfile,"%s ",PCEnttype(p));
/*          rgENTTYPES[p->kwType]); */
    { char * pcT;
      char * qT;
      pcT = PCFromPLLL(p->plllInt);
      if (strchr(pcT,'\"') == NULL) {
        qT = "\"";
      } else {
        qT = "\'";
      }
      fprintf(pfile,"%s%s%s",qT,pcT,qT);
    }
  }
};

char * PCDoMskw(enum kwMSTYPES kw) {
  char * pc;
  pc = mycat2(" ",rgKwms[kw]);
  f  = (f > kw ? f : kw);
  return pc;
};

/* The function occurs works like a (symmetrical) table:

         1    ?    +    *
       --------------------
    1  | 1    ?    +    *
    ?  | ?    ?    *    *
    +  | +    *    +    *
    *  | *    *    *    *

*/

char occurs(char cOcc1, char cOcc2) {
    if (cOcc1=='1') {
       return cOcc2;
    } else if (cOcc2=='1') {
       return cOcc1;
    } else if ((cOcc1=='*') || (cOcc2=='*')) {
       return '*';
    } else if (cOcc1==cOcc2) {
       return cOcc1;
    } else {
       return '*';
    }
};
/* if we cared about speed we could do something clever with bit masks.
*/

/* status is also a simple table:

    occ  1    ?    +    *
    stat
    0    e    0    e    0
    1    1    1    1    1
    2    2    2    2    2
    e    e    0    e    0
    */

#define OKSTATCH(C) ((C=='1')||(C==',')||(C=='|')||(C=='&'))
char status(char cStat, char cOcc) {
    if (OKSTATCH(cStat)) {
         return cStat;
    } else if (cOcc=='1' || cOcc=='+') {
         return 'e';
    } else if (cOcc=='?' || cOcc=='*') {
         return '0';
    } else {
      /* this should never happen */
         return 'e';
    }
};

char * EClotheE(char * pcExpression) {
  char * pcRes = mycat2("  ",SClotheE(pcExpression));
  OCC(pcRes) = OCC(pcExpression);
  STATUS(pcRes) = STATUS(pcExpression);
  return pcRes;
};

char * SClotheE(char * pcExpression) {
  char * pcRes;

  if (STATUS(pcExpression)=='1') {
    if (OCC(pcExpression)=='1') {
       pcRes = mycopy1(pcExpression+2);
    } else if (OCC(pcExpression)=='?') {
       pcRes = mycat2(pcExpression+2,"?");
    } else if (OCC(pcExpression)=='*') {
       pcRes = mycat2(pcExpression+2,"*");
    } else if (OCC(pcExpression)=='+') {
       pcRes = mycat2(pcExpression+2,"+");
    }
  } else {
    if (OCC(pcExpression)=='1') {
       pcRes = mycat3("(",pcExpression+2,")");
    } else if (OCC(pcExpression)=='?') {
       pcRes = mycat3("(",pcExpression+2,")?");
    } else if (OCC(pcExpression)=='*') {
       pcRes = mycat3("(",pcExpression+2,")*");
    } else if (OCC(pcExpression)=='+') {
       pcRes = mycat3("(",pcExpression+2,")+");
    }
  }
  return pcRes;
};

#define OKSTAT(S) ((S[1]=='1')||(S[1]==',')||(S[1]=='|')||(S[1]=='&'))
#define NEEDSWRAP(S,C) (((S[1]!='1')&&(S[0]!='1'))||(S[1]!=C))

char * EBuildseqEEK(char * pc1, char * pc3, char cConn) {
    char * pcRes;
    char * pcConn;

    if (cConn==',') {
      pcConn = ", ";
    } else if (cConn=='&') {
      pcConn = " & ";
    } else {
      pcConn = " BADCONNECTOR ";
    };

    if (STATUS(pc1)=='0') {
       free(pc1);
       return pc3;
    } else if (STATUS(pc3)=='0') {
       free(pc3);
       return pc1;
    } else if ((STATUS(pc1)=='e') || (STATUS(pc3)=='e')) {
       pcRes = mycat3(EClotheE(pc1),pcConn,SClotheE(pc3));
       OCC(pcRes) = '1';
       STATUS(pcRes) = 'e';
       free(pc1);
       free(pc3);
       return pcRes;
    } else {
       assert(OKSTAT(pc1));
       assert(OKSTAT(pc3));
       if (NEEDSWRAP(pc1,cConn)) {
	 pc1 = EClotheE(pc1);
       }
       if (NEEDSWRAP(pc3,cConn)) {
	 pc3 = EClotheE(pc3);
       }
       pcRes = mycat3(pc1,pcConn,pc3+2);
       OCC(pcRes) = '1';
       STATUS(pcRes) = cConn;
       free(pc1);
       free(pc3);
       return pcRes;
    }
};

char * EBuildaltEE(char * pc1, char * pc3) {
    char * pcRes;

    if ((STATUS(pc1)=='0') || (STATUS(pc1)=='e')) {
       free(pc1);
       return pc3;
    } else if ((STATUS(pc3)=='0') || (STATUS(pc3)=='e')) {
       free(pc3);
       return pc1;
    } else {
       assert(OKSTAT(pc1));
       assert(OKSTAT(pc3));
       if (NEEDSWRAP(pc1,'|')) {
	 pc1 = EClotheE(pc1);
       }
       if (NEEDSWRAP(pc3,'|')) {
	 pc3 = EClotheE(pc3);
       }
       pcRes = mycat3(pc1," | ",pc3+2);
       OCC(pcRes) = '1';
       STATUS(pcRes) = '|';
       free(pc1);
       free(pc3);
       return pcRes;
    }
};


