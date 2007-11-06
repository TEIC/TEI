%{
/* fpiresl.l -- flex input for scanner for FPI resolver */
/*  To do:
- when yacc grammar can handle it, make peref recognition work the same
  in all modes where perefs are legal (i.e. return PEREF, EE to yacc)
*/
/*  Revisions:
** 1999-02-21 : CMSMcQ : began file
*/

#include "fpires.tab.h"
#include "mycat.h"
#include "msg.h"

#define SAVEVALUE yylval.s = mycopy1(yytext)

/* Zortech thinks fileno is not ANSI C, Vern Paxson thinks it is */
/* #define fileno(fp)	((fp)->_file) */

char* pc;
int cLinecount;
int fWSseen;
#define max(A,B)  (A > B) ? A : B

/*
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
    if ( (result = ReadEntity(buf,max_size)) < 0 ) \
        YY_FATAL_ERROR( "read() in flex scanner failed" )
*/
#define CON 0
%}

%s PUBID LIT LITA

/* Classes of characters */
Digit         [0-9]
LCLetter      [a-z]
UCLetter      [A-Z]
Letter        {LCLetter}|{UCLetter}
Namechar      {Letter}|{Digit}|[\.\-]
NonLetter     [^a-zA-Z]
NonNamechar   [^a-zA-Z0-9\.\-\;\n\r]
/* WhiteSpace    [ \t\n\r] */
WhiteSpace    [ \t\r]
Newline       \n
Name          {Letter}{Namechar}*

/* Delimiters */
/* 1. toggle delimiters */
com           "--"
lit           "'"
lita          "\""
comchar       [^-\n]
/* comstring     ({comchar}+"-"?{comchar}+)* */
comstring     ({comchar}*("-"{comchar}+)*)
/* litchar       [^']   */
/* litachar      [^"]   */
litchar       [^'%]
litachar      [^"%]
litpechar     [^%]

/* 7.  Literal strings ' */
litstring     ({litchar}*({FalsePero}+{litchar}+)*)
litastring    ({litachar}*({FalsePero}+{litachar}+)*)
litpestring   ({litpechar}*({FalsePero}+{litpechar}+)*)

/* 8.  Nested Marked Section starts */
NestedMSStart {mdo}{dso}{NonDSO}*{dso}
NestedMSString [^\n\]<]*

%%

"PUBLIC"                     { return(KWPUBLIC); }
"SYSTEM"                     { return(KWSYSTEM); }
"ENTITY"                     { return(KWENTITY); }
"CATALOG"                    { return(KWCATALOG); }
{com}                        { BEGIN COMMENT; }
<COM>{comchar}               { ; }
<COM>{com}                   { BEGIN 0; }
<COM>"\n"                    { cLinecount++; }

{lit}                        { lexbegin(LIT); return(LITDELIM);}
{lita}                       { lexbegin(LITA); return(LITDELIM); }
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
