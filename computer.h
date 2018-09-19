
#define MAXNUMINSTRS 1024	/* max # instrs in a program */
#define MAXNUMDATA 3072		/* max # data words */

struct SimulatedComputer {
    int memory [MAXNUMINSTRS+MAXNUMDATA];
    int registers [32];
    int pc;
    int printingRegisters, printingMemory, interactive, debugging;
};
typedef struct SimulatedComputer Computer;

typedef enum { R=0, I, J } InstrType;

typedef struct {
  int rs;        /* register numbers for rs, rt and rd */
  int rt;
  int rd;
  int shamt      // value for shift amount
  int funct;     // value for function code
} RRegs;

typedef struct {
  int rs;        /* register numbers for rs and rt */
  int rt;
  int addr_or_immed; // value of address or immediate
} IRegs;

typedef struct {
  int target;   // value of target address
} JRegs;

typedef struct {
  InstrType type; //R, J, or I
  int op;         //instruction name?
  union {
    RRegs r; //5ints
    IRegs i; //3ints
    JRegs j; //1int
  } regs;
} DecodedInstr;

typedef struct {
  int R_rs; /*Value in register rs*/
  int R_rt; /*Value in register rt*/
  int R_rd; /*Value in register rd*/
} RegVals;

void InitComputer (FILE*, int printingRegisters, int printingMemory,
    int debugging, int interactive);
void Simulate ();
