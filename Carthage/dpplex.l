%{
/* DPPlex.l -- flex input for scanner for DTD pre-processor */
/*  To do:
- when yacc grammar can handle it, make peref recognition work the same
  in all modes where perefs are legal (i.e. return PEREF, EE to yacc)
*/
/*  Revisions:
** 16 Oct 97 CMSMcQ recognize MDC in INITIAL mode, to ease recovery from 
**                  catastrophic error (user submits an SGML document 
**                  instance, not a DTD)
** 20 Jan 95 CMSMcQ recognize some declarations in CONTENT mode, so
**                  we can handle DTD fragments, as well as full DTDs
** 19 Aug 94 CMSMcQ get literals to work right; add marked sections
** 18 Aug 94 CMSMcQ suppress in-declaration comments silently
** 16 Aug 94 CMSMcQ revise peref recognition using new flex.
**                  add peref recognition within declarations
**                  move escape(string) function calls to yacc
** 14 Aug 94 CMSMcQ add first cut at recognition of perefs in DS
**                  (recognition is OK, it's the processing that's bad)
**  6 Aug 94 CMSMcQ add specialized scan states to prevent recognizing
**                  keywords when they ought to be scanned as NAME.
**                  Then remove them again (didn't work, and found
**                  what looks like a simpler solution).
**  4 Aug 94 CMSMcQ modify definition of comment string
**  2 Aug 94 CMSMcQ begin from old p2dtdlex.l
*/

#include <stdlib.h>
#include "lllit.h"
#include "carthago.h"
#include "sgmldtd.h"
#include "mycat.h"
#include "msg.h"
#include "hash.h"
extern HASHTABLE htDelenda;

enum kwMSTYPES { MS_TEMP, MS_INCLUDE, MS_RCDATA, MS_CDATA, MS_IGNORE };

#define SAVEVALUE yylval.s = mycopy1(yytext)
int FOpenEntity(char *name);
char *PCCloseentity(void);
int ReadEntity(void *buf, int max_size);
int cEntCur = 0;
int cMSNesting = 0;
int kwMSLevel = 0;
extern int fRdexternal;

/* if yywrap doesn't return 1, EOF rule is not called!
** Does the documentation really say or imply this? */
/* #undef yywrap                       */
/* #define yywrap() (cEntCur <= 0)     */
/* Zortech thinks fileno is not ANSI C, Vern Paxson thinks it is */
/* #define fileno(fp)	((fp)->_file) */
char* pc;
int cLinecount;
#define max(A,B)  (A > B) ? A : B

/* #undef YY_INPUT */
/* #define YY_INPUT(buf,result,max_size) \
**    result = (buf[0] = getc(yyin)) == EOF ? YY_NULL : 1;
*/

/*
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
    if ( (result = read( fileno(yyin), (char *) buf, max_size )) < 0 ) \
        YY_FATAL_ERROR( "read() in flex scanner failed" ); \
    if (result == 0) { \
         fprintf(stderr,"... End of file encountered ...\n"); \
         fprintf(stderr,"cEntCur = %d\n",cEntCur);    \
    }
*/
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
    if ( (result = ReadEntity(buf,max_size)) < 0 ) \
        YY_FATAL_ERROR( "read() in flex scanner failed" )
/*  ReadEntity is defined in entmgr.c -- it can be a macro if we
**  find it's too slow, but for now, a function
*/
#define CON 0
%}

%s DS DSMS GRP LIT MD PI REF TAG
%s CMT MSK LITA LITPE MSIGNORE
%x READDELENDA SKIPREPORT
/*
** ISO 8879 clause 9.6.1 defines these recognizer modes, which we use
** here:
** CON  = content (default, unmarked),
** DS   = declaration subset
** DSMS = marked section within declaration subset
** GRP  = group
** LIT  = literal
** MD   = markup declarations
** PI   = processing instructions
** REF  = entity references (i.e. during reading of entity name)
** TAG  = tag (i.e. during start-tag and end-tag in document)
** ISO 8879 also defines a mode named CXT, which is not necessary here:
** Lex takes care of it with specifications for trailing context.
**
** In order to avoid having to add more modes for special cases (such as
** the mode needed to recognize '?' '+' or '*' immediately after the
** closing ')' of a model group), we define some lexical tokens not
** used in 8879 (e.g. ')?' ')+' and ')*').
**
** Even so, we do add the following modes not listed as recognizer modes
** in 8879, but described in its text:
** CMT  = comment (recognizes *only* end of comment) ?
** MSK  = in start of marked section (to recognize keywords)
** LITA = literals which started with lita
** LITPE = parameter entities within literals
** MSIGNORE = within an ignored marked section (recognize only ms start,
**       ms close)
**
** We must also prevent misreading the following strings as keywords
** in contexts where they should be recognized as NAMEs, not as keywords:
**
** SYSTEM, PUBLIC (when not parsing external identifiers)
** O, o (and minus?) (when not parsing omissibility)
** CDATA, RCDATA, EMPTY, ANY (when not parsing content model or
**      declared content)
** ENTITY, ENTITIES, ID, IDREF(S), NAME(S), NMTOKEN(S), NUMBER(S),
** NUTOKEN(S), NOTATION (when not parsing declared value of attributes)
** SDATA, PI, STARTTAG, ENDTAG, MS, MD, NDATA, SUBDOC (when not
**      parsing entity declarations
**
** I tried and failed to get by with declaring just one extra mode,
** MDNAME, in which no keywords are recognized, leaving MD capable of
** recognizing keywords.  This is not quite enough for attribute lists,
** and I considered interfering with the scan mode from inside the Bison
** rules ('no, yecch'), and tried to keep track of the parse state here,
** with variables to keep track of whether I was about to read
** the attribute name, the declared value, or the default value.
** This failed, though I couldn't see why, so I changed the Bison
** grammar to accept all keywords as names, where appropriate.
**
** Yecch either way.  This whole thing only occurs because they don't
** want to use the RNI when they don't absolutely have to.  A good thing
** to change in a successor language.
*/

/* Classes of characters */
Digit         [0-9]
LCLetter      [a-z]
UCLetter      [A-Z]
Letter        {LCLetter}|{UCLetter}
Namechar      {Letter}|{Digit}|[\.\:\-]
NonLetter     [^a-zA-Z]
NonNamechar   [^a-zA-Z0-9\.\-\;\n\r]
/* WhiteSpace    [ \t\n\r] */
WhiteSpace    [ \t\r]
Newline       \n

/* Three primitive types of tokens.  These are not identical to
** those specified by the SGML DTD grammar.  Ours map to theirs thus:
**
**      SGML               Ours
**      number           = number
**      name             =           name
**      name-token       = number or name or numtok
**      number-token     = number         or numtok
**
** I.e. yacc should define name-token as number | name | numtok, and
** number-token as number | numtok.
*/

Number        {Digit}+
Name          {Letter}{Namechar}*
NumTok        {Digit}+([\.\-]|{Letter}){Namechar}*

/* Delimiters */
/* 1. open and close delimiter pairs */
mdo           "<!"
NonMDO        [^<]
mdc           ">"
grpo          "("
grpc          ")"
pio           "<?"
pic           ">"
pichar        [^>]
pistring      {pichar}*
dso           "["
NonDSO        [^[]
dsc           "]"
msc           "]]"
NonMSC        [^\]]
dtgo          "["
dtgc          "]"
ero           "&"
pero          "%"
/* pero is also used in litchar, litachar below */
cro           "&#"
refc          ";"
stago         "<"
etago         "</"
tagc          ">"

/* 2. toggle delimiters */
com           "--"
lit           "'"
lita          "\""
comchar       [^-]
/* comstring     ({comchar}+"-"?{comchar}+)* */
comstring     ({comchar}*("-"{comchar}+)*)
/* litchar       [^']   */
/* litachar      [^"]   */
litchar       [^'%]
litachar      [^"%]
litpechar     [^%]

/* 3.  occurrence and sequence operators, for content models and
exceptions */

and           "&"
or            "|"
seq           ","
opt           "?"
plus          "+"
rep           "*"
minus         "-"
occurrence    ({opt}|{plus}|{rep})

/* 4.  other */
rni           "#"
vi            "="
net           "/"

/* 5.  keywords */
/*
system        [Ss][Yy][Ss][Tt][Ee][Mm]
public        [Pp][Uu][Bb][Ll][Ii][Cc]
*/

/* 6.  parameter entity references */
PeRef         {pero}{Name}({refc}|\n|\r\n)?
FalsePero     {pero}{NonLetter}

/* 7.  Literal strings */
litstring     ({litchar}*({FalsePero}+{litchar}+)*)
litastring    ({litachar}*({FalsePero}+{litachar}+)*)
litpestring   ({litpechar}*({FalsePero}+{litpechar}+)*)

/* 8.  Nested Marked Section starts */
NestedMSStart {mdo}{dso}{NonDSO}*{dso}
NestedMSString [^\n\]<]*

%%

"<undeclared>"               {    lexbegin(READDELENDA);
                                  return(CARTHAGO);
                             }
<READDELENDA>{Name}          {    SAVEVALUE; return(DELETED);  }
<READDELENDA>.               {    ;    }
<READDELENDA>"\n"            {    ;    }
<READDELENDA>"</undeclared>" {    lexend(READDELENDA); return(EST);  }
"<used>"                     {    lexbegin(SKIPREPORT); }
"<unused>"                   {    lexbegin(SKIPREPORT); }
"<declared>"                 {    lexbegin(SKIPREPORT); }
<SKIPREPORT>"."              {    ;    }
<SKIPREPORT>"\n"             {    ;    }
<SKIPREPORT>"</used>"        {    lexend(SKIPREPORT); }
<SKIPREPORT>"</unused>"      {    lexend(SKIPREPORT); }
<SKIPREPORT>"</declared>"    {    lexend(SKIPREPORT); }


    /* Beginning and ending MD (markup declaration) mode:  compound
    ** delimiters and mdc (markup declaration close).  We recognize doctype
    ** declarations in content mode, but element, attlist, entity, and
    ** notation declarations only in declaration subset mode (in or out of a
    ** marked section).  Those declarations put us in
    */

{mdo}"DOCTYPE"               {    lexbegin(MD);
                                  return(MDDOCTYPE);
                             }
<DS,DSMS>{mdo}"ELEMENT"      {    lexbegin(MD);
                                  return(MDELEMENT);
                             }
<DS,DSMS>{mdo}"ATTLIST"      {    lexbegin(MD);
                                  return(MDATTLIST);
                             }
<DS,DSMS>{mdo}"ENTITY"       {    lexbegin(MD);
                                  return(MDENTITY);
                             }
<DS,DSMS>{mdo}"NOTATION"     {    lexbegin(MD);
                                  return(MDNOTATION);
                             }

    /* now the same ones, but the first time we recognize them,
    ** we go into DS.  Adding the following rules allows us to
    ** accept DTD fragments, as well as whole DTDs.
    */

<INITIAL>{mdo}"ELEMENT"      { lexbegin(DS); lexbegin(MD); 
                               return(MDELEMENT); 
                             }
<INITIAL>{mdo}"ATTLIST"      { lexbegin(DS); lexbegin(MD);
                               return(MDATTLIST); 
                             }
<INITIAL>{mdo}"ENTITY"       { lexbegin(DS); lexbegin(MD); 
                               return(MDENTITY); 
                             }
<INITIAL>{mdo}"NOTATION"     { lexbegin(DS); lexbegin(MD); 
                               return(MDNOTATION); 
                             }
 
<MD,INITIAL>{mdc}            {    lexend(MD);
                                  return(MDC);
                             }

    /* Beginning and ending DS (declaration subset) mode:  DSO, DSC
    ** (declaration subset open, close).  Note that when we encounter
    ** '<!DOCTYPE' we enter markup declaration mode, and if we then
    ** encounter '[' we enter declaration subset mode.
    ** DSO and DSC may also be encountered in entity declarations,
    ** surrounding an attribute specification list for an entity
    ** of type CDATA, NDATA, or SDATA.
    */

<MD>{dso}                    {    lexbegin(DS); return(DSO); }
<DS>{dsc}                    {    lexend(DS); return(DSC);   }

    /* Beginning and ending GRP mode:  GRPO, GRPC, compounds  */
    /* The compound openers are for content model exceptions. */
    /* The compound closers are for content models.           */
<GRP,MD>{grpo}          { lexbegin(GRP); return(GRPO); }
<MD>{plus}{grpo}        { lexbegin(GRP); return(PLUSGRP); }
<MD>{minus}{grpo}       { lexbegin(GRP); return(MINUSGRP); }
<GRP>{grpc}             { lexend(GRP);
                          return(GRPC); }
<GRP>{grpc}{opt}        { lexend(GRP);   return(GRPOPT); }
<GRP>{grpc}{plus}       { lexend(GRP);   return(GRPPLUS); }
<GRP>{grpc}{rep}        { lexend(GRP);   return(GRPREP); }

    /* In Groups (name groups, content models) */
<GRP>{and}              { return(AND); }
<GRP>{or}               { return(OR); }
<GRP>{seq}              { return(SEQ); }
<GRP>{opt}              { return(OPT); }
<GRP>{plus}             { return(PLUS); }
<GRP>{rep}              { return(REP); }
<GRP>{rni}"PCDATA"      { return(RNIPCDATA); }

    /* Beginning and ending COMMENT mode. */
    /* in GRP, PI, REF, TAG, LIT we don't recognize comments */
    /* in CMT, we recognize only the end of a comment */
    /* in MD, MSK we suppress comments silently   */
    /* (in MSK, should they be recognized at all?) */
    /* in CON, DS, DSMS we recognize comment declarations
    ** i.e. comments of form <!-- ... -->,
    ** and return them to yacc
    */
<DS,DSMS,INITIAL>{mdo}{com}  {
                                  lexbegin(MD); lexbegin(CMT);
                                  return(MDOCOM);
                             }
<DS,DSMS,INITIAL>{mdo}{mdc}  {    return(MDOMDC);     }

    /* In comments */
<CMT>{comstring}        { /*
                          fprintf(stderr,"Comment string:  '%s'\n",yytext);
                          */
                          SAVEVALUE;  return(STRING); }
<CMT>{com}              { lexend(CMT);   return(COM); }

    /* {mdo}{com}       { lexbegin(MD); lexbegin(CMT);
    **                       return(MDOCOM);
    **                  }
    */
    /* <MD>{com}        { lexbegin(CMT); return(COM); }
    */

<MD,MSK>{com}{comstring}*{com} { /* 
                                 fprintf(stderr,
                                 "Igncomm string:  '%s'\n",yytext);
                                 */
                          /* do nothing */ }


    /* Beginning and ending Processing Instructions */
{pio}                   { lexbegin(PI); return(PIO); }
<MD,DS>{pio}            { lexbegin(PI); return(PIO); }
<PI>{pic}               { lexend(PI); return(PIC); }

    /* in processing instructions */
<PI>{pistring}          { SAVEVALUE; return(STRING); }

    /* Special things for all markup declarations */
    /* <MD>{lit}{litstring}{lit}       { SAVEVALUE; return(LITERAL); } */
    /* <MD>{lita}{litastring}{lita}    { SAVEVALUE; return(LITERAL); } */
    /* literals are recognized in DS -- not between declarations,
         but within data attribute specs (DS within MD) */
<MD,DS>{lit}                      { lexbegin(LIT); return(LITDELIM);}
<MD,DS>{lita}                     { lexbegin(LITA); return(LITDELIM); }
<MD>"SYSTEM"                      { SAVEVALUE; return(KWSYSTEM);  }
<MD>"PUBLIC"                      { SAVEVALUE; return(KWPUBLIC);  }
    /* if we wanted, we could use yymore() to collect the entire
    ** literal before returning to yacc.  But we'll want to break
    ** out parameter entity references in version 2 anyway, and
    ** anyone who doesn't want them can ignore them.  So we won't
    ** collect the string, but return it in pieces (sans quotes).
    */
<LIT>{litstring}                  { SAVEVALUE; return(LITERAL);   }
<LITA>{litastring}                { SAVEVALUE; return(LITERAL);   }
<LITPE>{litpestring}              { SAVEVALUE; return(LITERAL);   }
<LIT>{lit}                        { lexend(LIT); return(LITDELIM); }
<LITA>{lita}                      { lexend(LITA); return(LITDELIM); }

    /* Special things for element declarations */
    /* CDATA also used in entity declaraions */
<MD>"O"              { SAVEVALUE; return(OH);        }
<MD>{minus}          { SAVEVALUE; return(MINUS);     }
<MD>"CDATA"          { SAVEVALUE; return(KWCDATA);   }
<MD>"RCDATA"         { SAVEVALUE; return(KWRCDATA);  }
<MD>"EMPTY"          { SAVEVALUE; return(KWEMPTY);   }
<MD>"ANY"            { SAVEVALUE; return(KWANY);     }

    /* Special things for attlist declarations */
<MD>{rni}"NOTATION"  { return(RNINOTATION); }
<MD>"ENTITY"         { SAVEVALUE; return(KWENTITY);    }
<MD>"ENTITIES"       { SAVEVALUE; return(KWENTITIES);  }
<MD>"ID"             { SAVEVALUE; return(KWID);        }
<MD>"IDREF"          { SAVEVALUE; return(KWIDREF);     }
<MD>"IDREFS"         { SAVEVALUE; return(KWIDREFS);    }
<MD>"NAME"           { SAVEVALUE; return(KWNAME);      }
<MD>"NAMES"          { SAVEVALUE; return(KWNAMES);     }
<MD>"NMTOKEN"        { SAVEVALUE; return(KWNMTOKEN);   }
<MD>"NMTOKENS"       { SAVEVALUE; return(KWNMTOKENS);  }
<MD>"NUMBER"         { SAVEVALUE; return(KWNUMBER);    }
<MD>"NUMBERS"        { SAVEVALUE; return(KWNUMBERS);   }
<MD>"NUTOKEN"        { SAVEVALUE; return(KWNUTOKEN);   }
<MD>"NUTOKENS"       { SAVEVALUE; return(KWNUTOKENS);  }
<MD>"NOTATION"       { SAVEVALUE; return(KWNOTATION);  }
<MD>{rni}"FIXED"     { return(RNIFIXED);    }
<MD>{rni}"REQUIRED"  { return(RNIREQUIRED); }
<MD>{rni}"CURRENT"   { return(RNICURRENT);  }
<MD>{rni}"CONREF"    { return(RNICONREF);   }
<MD>{rni}"IMPLIED"   { return(RNIIMPLIED);  }

    /* Special things for entity declarations */
<MD>{rni}"DEFAULT"   { return(RNIDEFAULT);   }
<MD>{pero}({WhiteSpace}|{Newline})+ {
                       for (pc = yytext; *pc ; pc++)
                             if (*pc == '\n') cLinecount++;
                       return(PEROSPACE);
                     }
    /*
    <MD>{pero}{WhiteSpace}+ { return(PEROSPACE); }
    */
<MD>"SDATA"          { SAVEVALUE; return(KWSDATA);      }
<MD>"PI"             { SAVEVALUE; return(KWPI);         }
<MD>"STARTTAG"       { SAVEVALUE; return(KWSTAG);       }
<MD>"ENDTAG"         { SAVEVALUE; return(KWETAG);       }
<MD>"MS"             { SAVEVALUE; return(KWMS);         }
<MD>"MD"             { SAVEVALUE; return(KWMD);         }
<MD>"NDATA"          { SAVEVALUE; return(KWNDATA);      }
<MD>"SUBDOC"         { SAVEVALUE; return(KWSUBDOC);     }
<MD>{vi}             { return(VI);           }
<DS>{vi}             { return(VI);           }

    /* Special rules for situations in which keywords require RNI,
    so keywords without RNI should be read as NAME, not as keywords */
    /* deleted ... */

    /* Recognizing ps: parameter entity references
    and entity ends  -- within markup declarations */
    /* in CON, PI, REF, TAG, CMT we don't expand p.e. refs */
    /* in MD and GRP, we expand p.e. refs silently   */
    /* in DS, DSMS, LIT, MSK we expand p.e. refs and return PEREF,EE */
<MD,GRP>{PeRef}  {
                   pc = PCCleanperef(yytext);
                   FOpenEntity(yytext);
                   /* instead of returning, just end silently. */
                   /* (we did try emitting sof data here, no good.)   */
                 }
<MD,GRP><<EOF>> {
                   pc = PCCloseentity();
                   if (pc == NULL)
                        yyterminate();
                 }
    /* parameter entities in literals move us into special mode, in
       which we don't recognize the end-literal delimiters */
<LIT,LITA,LITPE>{PeRef} {
                   pc = PCCleanperef(yytext);
                   FOpenEntity(yytext);
                   lexbegin(LITPE);
                   yylval.s = pc;
                   return(PEREF);
                 }
<LITPE><<EOF>> {
                   pc = PCCloseentity();
                   lexend(LITPE);
                   if (pc == NULL)
                        yyterminate();
                   else {
                        yylval.s = mycopy1(pc);
                        return(EE);
                   }
                 }

    /* entity boundaries between markup declarations */
<DS,DSMS,MSK>{PeRef} {
                   pc = PCCleanperef(yytext);
                   yylval.s = pc;
                   if (FOpenEntity(yytext)) {
                        return(PEREF);
                   } else {
                        return(PEREFERROR);
                   }
                 }
<DS,DSMS,MSK><<EOF>> {
                   pc = PCCloseentity();
                   if (pc == NULL)
                        yyterminate();
                   else {
                        yylval.s = mycopy1(pc);
                        return(EE);
                   }
                 }

    /* Marked Sections */
    /* enum kwMSTYPES {
       MS_TEMP, MS_INCLUDE, MS_RCDATA, MS_CDATA, MS_IGNORE };
       f  = (f > kw ? f : kw);
    */

<DS,DSMS>{mdo}{dso}     { lexbegin(MSK);
                          kwMSLevel = MS_TEMP;
                          return(MDODSO);
                        }
{mdo}{dso}              { lexbegin(DS); lexbegin(MSK);
                          kwMSLevel = MS_TEMP;
                          return(MDODSO);
                          /* duplicate for DTD fragments */
                        }
<MSK>{dso}              { lexend(MSK);
                          if (kwMSLevel == MS_IGNORE && fMSIgnore) {
                             lexbegin(MSIGNORE);
                             MsgKwS(msgDEBUG,"Flex: Starting MSIGNORE",NULL);
                             cMSNesting = 1;
                          } else
                             lexbegin(DSMS);
                          return(DSO); }
<MSK>"TEMP"             { return(KWTEMP);     }
<MSK>"INCLUDE"          { kwMSLevel = max(kwMSLevel,MS_INCLUDE);
                          return(KWINCLUDE);  }
<MSK>"RCDATA"           { kwMSLevel = max(kwMSLevel,MS_RCDATA);
                          return(KWRCDATA);   }
<MSK>"CDATA"            { kwMSLevel = max(kwMSLevel,MS_CDATA);
                          return(KWCDATA);    }
<MSK>"IGNORE"           { kwMSLevel = max(kwMSLevel,MS_IGNORE);
                          return(KWIGNORE);   }
<DSMS>{msc}{mdc}        {
                          MsgKwS(msgDEBUG,"Flex: Ending MS",NULL);
                          lexend(DSMS); return(MSCMDC);     }
    /* within an ignored marked section, we recognize only
       marked-section beginnings and endings, and throw
       everything else away.  In particular, we can ignore
       perefs and entity ends, since the marked section must end
       within the current entity.
    */
<MSIGNORE>{mdo}{dso}{NonDSO}*{dso}     { cMSNesting++; }
<MSIGNORE>{mdo}              { /* false nested MS start */ }
<MSIGNORE>{msc}{mdc}         { if (--cMSNesting == 0) {
                                  lexend(MSIGNORE);
                                  return(MSCMDC);
                               }
                             }
<MSIGNORE>{dsc}              { /* false ms close */        }
<MSIGNORE>[^\n\]<]*          { /* string */                }
<MSIGNORE>{Newline}          { cLinecount++; }

    /* Universals */
{Name}          {  /* strcpy(yylval.s,yytext); */
                   SAVEVALUE; 
                   if (PPRLookupKeyHt(yylval.s,htDelenda) == NULL) {
		     return(NAME);
		   } else {
		     return(DELETED);
		   }
                }
{Number}        {  /* strcpy(yylval.s,yytext); */
                   SAVEVALUE; return(NUMBER);
                }
{NumTok}        {  /* strcpy(yylval.s,yytext); */
                   SAVEVALUE; return(NUMTOK);
                }

[ \t\r\x1a]     ;
{Newline}       { cLinecount++;
                }
    /* {WhiteSpace}+   ;
    */
    /*
    <DS,DSMS,GRP,LIT,MD,PI,REF,TAG,CMT,MSK>{WhiteSpace}+   ;
    <MD>{WhiteSpace}+   ;
    */
%%

/*********************************************************************/
/* Lexbegin and related stack routines                               */
/*********************************************************************/
#define LSTACK 100
static int lexstack[LSTACK];   /* stack of lex parsing states */
static int *piStack;           /* stack pointer */

extern void abort();
void initfstack(void) {
        piStack = lexstack;
}

#undef DEBUGGING  

void pushstack(int cond) {
        if (piStack >= &lexstack[LSTACK]) {
                fprintf(stderr,"Push:  Lex state stack overflow");
                abort();
        }
#ifdef DEBUGGING
        fprintf(stderr,"pushstack: Pushing Lex state %d\n",cond);
#endif
        *piStack++ = cond;
}

int popstack(void) {
        if (piStack <= lexstack) {
                fprintf(stderr,"Pop: Lex state stack underflow\n");
                abort();
        }
#ifdef DEBUGGING
        fprintf(stderr,"popstack:  Popping a Lex state \n");
#endif
        return *--piStack;
}
void lexbegin(int cond) {
/* put a state on the stack and begin it */
#ifdef DEBUGGING
        fprintf(stderr,"lexbegin: Beginning Lex state %d\n",cond);
#endif
        pushstack(cond);
        BEGIN cond;
}
void lexend(int cond) {
/* end the current state and revert to the next lower scan state
   in the parse.  Usually the current state should be the one
   passed as a parameter.  If it's not, pop states until you find
   the one you're looking for.
   Then start the next one (using lexbegin to put it back on the stack)
*/
        int s;

#ifdef DEBUGGING
        fprintf(stderr,"lexend: Ending Lex state %d\n",cond);
#endif
        if (cond == 0) {
                s = popstack();
#ifdef DEBUGGING
        fprintf(stderr,"lexend: Called with 0, popping state %d\n",cond);
#endif
        }
        else {
                do {
                        s = popstack();
#ifdef DEBUGGING
        fprintf(stderr,"lexend: Top of stack has state %d, popped now\n",s);
#endif
                } while (s != cond);
        }
#ifdef DEBUGGING
        fprintf(stderr,"lexend:  We have found state %d, restarting next\n",s);
#endif
        s = popstack();
        lexbegin(s);
}
int lexpeek(void) {
        if (piStack <= lexstack) {
                fprintf(stderr,"Lexpeek:  Lex state stack underflow\n");
                abort();
        }
#ifdef DEBUGGING
        fprintf(stderr,"Lexpeek:  Peeking at Lex stack, found state %d\n",
              *(piStack-1));
#endif
        return *(piStack - 1);
}
#include "entmgr.c"
