void initfstack(void);
void lexbegin(int cond);
void lexend(int cond);
int  lexpeek(void);
extern int cLinecount;

#define INITIAL 0
#define CON 0
#define DS 2
#define DSMS 4
#define GRP 6
#define LIT 8
#define MD 10
#define PI 12
#define REF 14
#define TAG 16
#define CMT 18
#define MSK 20
#define ENTREF 22
