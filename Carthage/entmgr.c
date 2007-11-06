/* EntMgr.c:  entity manager code for SGML parsers */
/* Revisions:
** 1995-09-01 : CMSMcQ : move fnCurrent to msg module from here.
** 1994-08-17 : CMSMcQ : fix bug (dereference pointer!) in FOpenEntity
**                       try to normalize variable names
**                       fix calls to mycopy1 (it does not return
**                         NULL on failure, it aborts)
** 1994-08-16 : CMSMcQ : revert to FOpen, PCClose with new flex
** 1994-08-14 : CMSMcQ : add FOpenEntity, PCCloseentity code
** 1994-08-12 : CMSMcQ : begin file, copying intall code from K/R
*/

/* #define DEBUGGING 1
*/
#ifndef FILESTATUS
#define FILESTATUS(F,S) \
    if (fDebug) \
    fprintf(stderr,"  %s has fileno=%d, feof=%d, ferror=%d\n",\
    S,fileno(F),feof(F),ferror(F)); \
    if (ferror(F)) perror(strerror(ferror(F)));
#endif
#include "mycat.h"
/* #include <stdlib.h> */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "myfiles.c"
#ifdef DEBUGGING
void PrintEntitytable(void);
#else
#define PrintEntitytable(A) {}
#endif
extern int fDebug;

/* define entity record structure */
/* define open-entity stack */

/* for now, the only parameter entities we will handle are those
declared in the forms <!ENTITY % x 'xx' > and <!ENTITY % x SYSTEM
'fileid'>.  That is, CDATA and SDATA internal entities, and CDATA,
SDATA, SUBDOC, and NDATA external entities, won't be loaded here.
*/

typedef struct entityrecord * PEntRec;
typedef struct entityrecord {/* (list of) parameter entity records */
    PEntRec perNext;         /* next entry in chain */
    char *pcEntname;         /* entity name, ge or %pe */
    char *pcEnttext;         /* internal replacement text, if any */
    int  fExtern;            /* external? */
    char *pcPubid;           /* public identifier, if any */
    char *pcSysid;           /* system identifier, if any */
} EntRec;

/* a null next pointer marks end of list. */
/* null enttext pointer iff external entity; nonnull iff internal. */
/* pubid and sysid are null or non-null, depending. */

/* #define HASHSIZE 101 */

static PEntRec rgEntrecs[HASHSIZE]; /* array of pe records */

unsigned hash(char *psz) {
    /* revised (131 not 31) from Gonnet/Baeza-Yates */
    /* copied from K&R p.144 */
    unsigned hashval;

    for (hashval = 0; *psz != '\0'; psz++)
         hashval = *psz + 131 * hashval;
    return hashval % HASHSIZE;
}

PEntRec PERLookup(char *pcName) {
    PEntRec per;

#ifdef DEBUGGING
    MsgKwS(msgDEBUG,"PERLookup called to find ",pcName,NULL);
    FILESTATUS(stdout,"stdout");
#endif
    for (per = rgEntrecs[hash(pcName)]; per != NULL; per = per->perNext)
         if (strcmp(pcName,per->pcEntname) == 0)
              return per;    /* found */
    return NULL;             /* not found */
}

/* FInstallEntity()
    returns 1 if OK,
    0 if error,
    -1 if entity is already declared / installed
*/

int FInstallEntity(char *pcName,
               char *pcText,
               int  fExt,
               char *pcPub,
               char *pcSys) {
    PEntRec per;
    unsigned hashval;

#ifdef DEBUGGING
    MsgKwS(msgDEBUG,"FInstallEntity called for ",pcName,NULL);
    FILESTATUS(stdout,"stdout");
#endif
    if ((per = PERLookup(pcName)) != NULL) {   /* found */
         return -1;                         /* don't touch a thing */
    } /* else */                            /* not found */
    per = (PEntRec ) malloc(sizeof(*per));
    if (per == NULL)
         return 0;
    hashval = hash(pcName);
    per->perNext = rgEntrecs[hashval];
    rgEntrecs[hashval] = per;

    per->pcEntname = mycopy1(pcName);
    per->fExtern = fExt;
    per->pcEnttext = per->pcPubid = per->pcSysid = NULL;
    if (fExt) {
         if (pcPub != NULL)
              per->pcPubid = mycopy1(pcPub);
         if (pcSys != NULL)
              per->pcSysid = mycopy1(pcSys);
    } else {
         per->pcEnttext = mycopy1(pcText);
    }
#ifdef DEBUGGING
    MsgKwS(msgDEBUG,"FInstallEntity installed ",pcName,NULL);
    FILESTATUS(stdout,"stdout");
    PrintEntitytable();
    FILESTATUS(stdout,"stdout");
#endif
    return 1;
}

#define cEntMax 32
static int cEntMac = 0;
extern char * fnCurrent;

/* int cEntCur = 0; */
/* cEntCur isn't static, it has to be visible in dpplex */

YY_BUFFER_STATE rgBuffers[cEntMax];
PEntRec  rgEntities[cEntMax];
int fRdexternal = 1;    /* are we reading an external file? */
char *rgPCFn[cEntMax];  /* and what are the names of open files? */
int  rgILine[cEntMax];  /* and the current line numbers? */
char *pcEntpos;         /* if not, where is the next character? */
char *rgPCPos[cEntMax]; /* and where were we in the other open entities? */

int FOpenEntity(char *pName) {
    PEntRec p;
    FILE *pf;
    char *pcT1;
    char *pcT2;
    char rgTempnames[L_tmpnam];

#ifdef DEBUGGING
    MsgKwS(msgDEBUG,"FOpenEntity called to open ",pName,NULL);
    FILESTATUS(stdout,"stdout");
    FILESTATUS(stdin,"stdin");
    FILESTATUS(stderr,"stderr");
    FILESTATUS(yyin,"yyin");
#endif
    /* suppress the pero and the trailing reference close, if any */
    /* N.B. this assumes the reference concrete syntax and is
       VERY DIRTY.  You hear me?  DIRTY, DIRTY, DIRTY!!
    */
    pName = PCCleanperef(pName);
/*  for (pcT1 = pcT2 = pName; *pcT1 ; pcT1++) {
         if ((*pcT1 != '%')  && (*pcT1 != ';')
          && (*pcT1 != '\n') && (*pcT1 != '\r') && (*pcT1 != '\t')) {
              *pcT2++ = *pcT1;
         }
    }
    *pcT2 = '\0';
*/
    if (cEntCur == cEntMax) {
         MsgKwS(msgERROR,"Cannot open entity ", pName,
                ", too many entities open",NULL);
         return 0;
    }
    rgPCPos[cEntCur]     = pcEntpos;
    rgILine[cEntCur]     = cLinecount;
    if (yyin == stdin)
         rgPCFn[cEntCur] = fnCurrent = "<stdin>";
    rgBuffers[cEntCur++] = YY_CURRENT_BUFFER;
    p = PERLookup(pName);
    if (p == NULL) {
         MsgKwS(msgERROR,"Could not open entity ",pName,
               " (no record for it)",NULL);
              return 0;
    }
    rgEntities[cEntCur] = p;
    if (p->fExtern) {
         MsgKwS(msgVERBOSE,"Opening entity ",pName," (",
                   p->pcSysid,")",NULL);
         yyin = PFileOpenFnEnvMode(p->pcSysid,"DPP_PATH","r");
         if (yyin == NULL) {
              MsgKwS(msgERROR,"Could not open entity ",pName,
                   " (file ",p->pcSysid,")",NULL);
              cEntCur--;
              return 0;
         }
         yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
#ifdef DEBUGGING
    FILESTATUS(stdout,"stdout");
    FILESTATUS(stdin,"stdin");
    FILESTATUS(stderr,"stderr");
    FILESTATUS(yyin,"yyin");
#endif
         fRdexternal = 1;
         pcEntpos = "";
         fnCurrent = rgPCFn[cEntCur] = p->pcSysid;
         cLinecount = 1;
         return 1;
    } else { /* handle internal entities */
         MsgKwS(msgTRACE,"Opening entity ",pName," (internal)",NULL);
         yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
         pcEntpos = p->pcEnttext;
         fRdexternal = 0;
         fnCurrent = rgPCFn[cEntCur] = mycat2("%", p->pcEntname);
         cLinecount = 1;
         return 1;
    }
};

char *PCCloseentity(void) {
    char * pc;
    PEntRec per;

#ifdef DEBUGGING
    FILESTATUS(stdout,"stdout");
    MsgKwS(msgTRACE,"Closing the current entity ...",NULL);
    MsgKwS(msgTRACE,"PCCloseentity() called ...",NULL);
    FILESTATUS(stdout,"stdout");
    FILESTATUS(stdin,"stdin");
    FILESTATUS(stderr,"stderr");
    FILESTATUS(yyin,"yyin");
#endif

    if (cEntCur > 0) {
         per = rgEntities[cEntCur];
         pc = per->pcEntname;
         if (per->fExtern)
              fclose(yyin);
         /* close yyin.  We don't need to reset it,
            that happens in yy_switch_to_buffer, below */
         yy_delete_buffer(YY_CURRENT_BUFFER);
         yy_switch_to_buffer(rgBuffers[--cEntCur]);
         /* reset pcEntpos and fRdexternal */
         if (cEntCur > 0) {
              per = rgEntities[cEntCur];
              if (per->fExtern) {
                   fRdexternal = 1;
                   pcEntpos = "";
              } else {
                   fRdexternal = 0;
                   pcEntpos = rgPCPos[cEntCur];
              }
         } else { /* back to stdin */
              fRdexternal = 1;
              pcEntpos = "";
         }
         cLinecount = rgILine[cEntCur];
         fnCurrent  = rgPCFn[cEntCur];


#ifdef DEBUGGING
    FILESTATUS(stdout,"stdout");
    FILESTATUS(stdin,"stdin");
    FILESTATUS(stderr,"stderr");
    FILESTATUS(yyin,"yyin");
#endif

         return pc;
    } else {
#ifdef DEBUGGING
    FILESTATUS(stdout,"stdout");
    FILESTATUS(stdin,"stdin");
    FILESTATUS(stderr,"stderr");
    FILESTATUS(yyin,"yyin");
#endif
         return NULL;
    }
}


/*
int yywrap(void) {
    if (cEntCur > 0) {
         fclose(yyin);
#ifdef DEBUGGING
         MsgKwS(msgDEBUG,"EOF, closed file ",
                   (rgEntities[cEntCur])->pcSysid,NULL);
#endif
         yyin = rgBuffers[--cEntCur];
#ifdef DEBUGGING
         MsgKwS(msgDEBUG,"Returning to ",
                   (rgEntities[cEntCur])->pcEntname,NULL);
#endif
**       YY_NEW_FILE; **
         return 0;
    } else {
#ifdef DEBUGGING
         MsgKwS(msgDEBUG,"Nothing left open ...\n",NULL);
#endif
         return 1;
    }

}
*/

#ifdef DEBUGGING
void PrintEntitytable(void) {
    PEntRec p;
    int i = 0;

    if ( ! fDebug) return;

    MsgKwS(msgDEBUG,"Printing Entity table ...\n",NULL);
    for (p = rgEntrecs[0]; i < HASHSIZE ; p = rgEntrecs[i++]) {
         if (p != NULL) {
              for ( ; p != NULL ; p = p->perNext) {
                   fprintf(stderr,"%d.  %s (external-flag %d)\n"
                        "  == internal %s\n"
                        "  == public %s\n"
                        "  == system %s\n",
                        i,p->pcEntname, p->fExtern,
                        p->pcEnttext, p->pcPubid,
                        p->pcSysid);
              }
         }
    }
}
#endif

#define min(A,B)  A < B ? A : B
int ReadEntity(void *buf, int max_size) {
    int iResult;

    if (fRdexternal) {
         iResult = read(fileno(yyin), (char *)buf, max_size);
    } else {
         iResult = min(max_size,strlen(pcEntpos));
         memcpy((void *)buf,(void *)pcEntpos,iResult);
         pcEntpos += iResult;
    }
    return iResult;
}
