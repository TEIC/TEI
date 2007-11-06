/* EntMgr.h:  declarations for entity manager code for SGML parsers */

struct lperec {              /* list of parameter entity records */
    struct lperec *plNext;   /* next entry in chain */
    char *pcEntname;         /* entity name, ge or %pe */
    char *pcEnttext;         /* internal replacement text, if any */
    int  fExtern;            /* external? */
    char *pcPubid;           /* public identifier, if any */
    char *pcSysid;           /* system identifier, if any */
};

int FInstallEntity(char *pcName,
               char *pcText,
               int  fExt,
               char *pcPub,
               char *pcSys);

int FOpenEntity(char *pName);

char *PCCloseentity(void);

void PrintEntitytable(void);

extern char * fnCurrent;
