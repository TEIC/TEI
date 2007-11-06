/* msg.h:  routines for messages (routine, log, verbose, debug ...) */

/* flags for DTD pre-processor */
extern int fTrace;           /* trace all actions */
extern int fDebug;           /* detailed messages */
extern int fVerbose;         /* verbose messages  */
extern int fMSIgnore;        /* ignore ignored MS */
extern int iMsglevel;        /* how verbose are we? */

enum MSGTYPES  { msgTRACE   = 0,
                 msgDEBUG   = 1,
                 msgVERBOSE = 3,
                 msgINFORM  = 5,
                 msgWARNING = 7,
                 msgERROR   = 10
};
extern char * fnCurrent;
extern int    cLinecount;

/* MsgKwS(kw,s,s, ...) takes one keyword and an arbitrary
   number of strings */
void MsgKwS(int kw, ...);
