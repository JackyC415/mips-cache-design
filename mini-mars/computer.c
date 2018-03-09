#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"

#undef mips            /* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);
void PrintInfo(int changedReg, int changedMem);
unsigned int Fetch(int);
void Decode(unsigned int, DecodedInstr *, RegVals *);
int Execute(DecodedInstr *, RegVals *);
int Mem(DecodedInstr *, int, int *);
void RegWrite(DecodedInstr *, int, int *);
void UpdatePC(DecodedInstr *, int);
void PrintInstruction(DecodedInstr *);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer(FILE *filein, int printingRegisters, int printingMemory,
                  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k = 0; k < 32; k++) {
        mips.registers[k] = 0;
    }

    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS + MAXNUMDATA) * 4;

    for (k = 0; k < MAXNUMINSTRS + MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
        /*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k > MAXNUMINSTRS) {
            fprintf(stderr, "Program too big.\n");
            exit(1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i >> 24) | (i >> 8 & 0x0000ff00) | (i << 8 & 0x00ff0000) | (i << 24);
}

/*
 *  Run the simulation.
 */
void Simulate() {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg = -1, changedMem = -1, val;
    DecodedInstr d;

    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf("> ");
            fgets(s, sizeof(s), stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch(mips.pc);

        printf("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /*
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode(instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /*
	 * Perform computation needed to execute d, returning computed value
	 * in val
	 */
        val = Execute(&d, &rVals);

        UpdatePC(&d, val);

        /*
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem,
	 * otherwise put -1 in *changedMem.
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /*
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo(changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo(int changedReg, int changedMem) {
    int k, addr;
    printf("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf("Updated r%2.2d to %8.8x\n",
               changedReg, mips.registers[changedReg]);
    } else {
        for (k = 0; k < 32; k++) {
            printf("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k + 1) % 4 == 0) {
                printf("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf("Updated memory at address %8.8x to %8.8x\n",
               changedMem, Fetch(changedMem));
    } else {
        printf("Nonzero memory\n");
        printf("ADDR	  CONTENTS\n");
        for (addr = 0x00400000 + 4 * MAXNUMINSTRS;
             addr < 0x00400000 + 4 * (MAXNUMINSTRS + MAXNUMDATA);
             addr = addr + 4) {
            if (Fetch(addr) != 0) {
                printf("%8.8x  %8.8x\n", addr, Fetch(addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch.
 */
unsigned int Fetch(int addr) {
    return mips.memory[(addr - 0x00400000) / 4];
}

/* Decode instr, returning decoded instruction. */
void Decode(unsigned int instr, DecodedInstr *d, RegVals *rVals) {
    /* Your code goes here */

    d->op = instr >> 26;

    //R-format
    if (!d->op) {
        // | opcode |   rs   |   rt   |   rd   |  shamt |  funct |
        d->type = R;

        /*computes rs,rt,rd,shamt & funct respectively*/
        d->regs.r.rs = (instr & 0x03ffffff) >> 21;
        d->regs.r.rt = (instr & 0x001fffff) >> 16;
        d->regs.r.rd = (instr & 0x0000ffff) >> 11;
        d->regs.r.shamt = (instr & 0x000007ff) >> 6;
        d->regs.r.funct = (instr & 0x0000003f);

        //updates register rs, rt & rd values
        rVals->R_rs = mips.registers[d->regs.r.rs];
        rVals->R_rt = mips.registers[d->regs.r.rt];
        rVals->R_rd = mips.registers[d->regs.r.rd];

        //J-Format; opcode w/ 2 or 3
    } else if (d->op == 2 || d->op == 3) {
        // |  opcode  |              address                    |
        d->type = J;

        //computes register target
        d->regs.j.target = (instr & 0x03ffffff) << 2;

        //else I-format
    } else {
        // |  opcode  |   rs   |   rt   |      immediate        |
        d->type = I;

        /*computes rs, rt & immediate respectively*/
        d->regs.i.rs = (instr & 0x03ffffff) >> 21;
        d->regs.i.rt = (instr & 0x001fffff) >> 16;
        d->regs.i.addr_or_immed = (instr & 0x0000ffff);

        //check for zero extend
        if (d->op == 0xc || d->op == 0xd) {
            d->regs.i.addr_or_immed = d->regs.i.addr_or_immed;

          //check for sign extend
        } else if (d->op == 0x9 || d->op == 0x23 || d->op == 0x2b) {
            if (d->regs.i.addr_or_immed < 0x8000) {
                d->regs.i.addr_or_immed = d->regs.i.addr_or_immed;
            } else {
                d->regs.i.addr_or_immed = d->regs.i.addr_or_immed | 0xffff0000;
                d->regs.i.addr_or_immed = -(!(d->regs.i.addr_or_immed) + 1);
            }
          //check for branches
        } else if (d->op == 0x4 || d->op == 0x5) {
            //computes address
            d->regs.i.addr_or_immed = (((mips.pc) + 4) + ((d->regs.i.addr_or_immed) << 2));
          //check for lui
        } else {
            if (d->regs.i.addr_or_immed < 0x8000) {
                d->regs.i.addr_or_immed = d->regs.i.addr_or_immed | 0x00000000;
            } else {
                d->regs.i.addr_or_immed = -(!(d->regs.i.addr_or_immed) + 1);
            }
        }

        //updates register rs & rt values
        rVals->R_rs = mips.registers[d->regs.r.rs];
        rVals->R_rt = mips.registers[d->regs.r.rt];
    }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction(DecodedInstr *d) {
    /* Your code goes here */

        //R-format
    if(d->type == R){

        //storing fields for more efficient printing later
        int rs=d->regs.r.rs;
        int rt=d->regs.r.rt;
        int rd=d->regs.r.rd;
        int shamt=d->regs.r.shamt;
        int funct=d->regs.r.funct;

        /*printing instructions addu,subu,sll,srl,and,or,slt & jr respectively*/
        if(funct==0x21)
            printf("addu\t$%d, $%d, $%d\n",rd,rs,rt);
        else if (funct==0x23)
            printf("subu\t$%d, $%d, $%d\n",rd,rs,rt);
        else if(funct==0x00)
            printf("sll\t$%d, $%d, %d\n",rd,rt,shamt);
        else if(funct==0x02)
            printf("srl\t$%d, $%d, %d\n",rd,rt,shamt);
        else if(funct==0x24)
            printf("and\t$%d, $%d, $%d\n",rd,rs,rt);
        else if(funct==0x25)
            printf("or\t$%d, $%d, $%d\n",rd,rs,rt);
        else if(funct==0x2a)
            printf("slt\t$%d, $%d, $%d\n",rd,rs,rt);
        else if(funct==0x08)
            printf("jr\t$%d\n",rs);//printing jr instruction
    }
        //I-format
    else if (d->type == I){

        int rs =d->regs.i.rs;
        int rt=d->regs.i.rt;
        int addr_or_immed=d->regs.i.addr_or_immed;

        /*printing instructions addiu,andi,ori,lui,beq,bne,lw & sw respectively*/
        if(d->op==0x9)
            printf("addiu\t$%d, $%d, %d\n",rt,rs,addr_or_immed);
        else if (d->op==0xc)
            printf("andi\t$%d, $%d, %d\n",rt,rs,addr_or_immed);
        else if (d->op==0xd)
            printf("ori\t$%d, $%d, %d\n",rt,rs,addr_or_immed);
        else if (d->op==0xf)
            printf("lui\t$%d, %d\n",rt,addr_or_immed);
        else if (d->op==0x4)
            printf("beq\t$%d, $%d, 0x%08X\n",rs,rt,addr_or_immed);
        else if (d->op==0x5)
            printf("bne\t$%d, $%d, 0x%08X\n",rs,rt,addr_or_immed);
        else if (d->op==0x23)
            printf("lw\t$%d, %d($%d)\n",rt,addr_or_immed,rs);
        else if (d->op==0x2b)
            printf("sw\t$%d, %d($%d)\n",rt,addr_or_immed,rs);
    }
        //J-format
    else if (d->type == J){

        /*printing j and jal instructions respectively*/
        if (d->op==0x2)
            printf("j\t 0x%08X\n",d->regs.j.target);
        else if (d->op==0x3)
            printf("jal\t 0x%08X\n",d->regs.j.target);
    }

}

/* Perform computation needed to execute d, returning computed value */
int Execute(DecodedInstr *d, RegVals *rVals) {
    /* Your code goes here */

    if(d->type == R){ //Instruction R
        int funct = d->regs.r.funct;
        if(funct == 0x21){		//ADDU R[rd] = R[rs] + R[rt]
            rVals->R_rd = rVals->R_rs + rVals-> R_rt;
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return  rVals-> R_rd;
        }else if(funct == 0x23){	 //SUBU R[rd] = R[rs] - R[rt]
            rVals->R_rd = rVals->R_rs - rVals-> R_rt;
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return  rVals-> R_rd;
        }else if(funct == 0x00){ 	//SLL R[rd] = R[rt] << shamt
            rVals->R_rd = rVals-> R_rt << d-> regs.r.shamt;
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return  rVals-> R_rd;
        }else if(funct == 0x02){ 	//SRL R[rd] = R[rt] >> shamt
            rVals->R_rd = (rVals-> R_rt >>  d-> regs.r.shamt) & ~(((0x1 << 31) >> d->regs.r.shamt) << 1);
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return  rVals-> R_rd;
        }else if(funct == 0x24){	 //AND R[rd] = R[rs] & R[rt]
            rVals-> R_rd = rVals-> R_rt & rVals->R_rs;
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return rVals-> R_rd;
        }else if(funct == 0x25){	//OR  R[rd] = R[rs] | R[rt]
            rVals-> R_rd = rVals-> R_rt | rVals->R_rs;
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return rVals-> R_rd;
        }else if(funct == 0x2a){	//SLT R[rd] = (R[rs] < R[rt]) ? 1 : 0
            if(rVals-> R_rs < rVals-> R_rt){
                rVals->R_rd = 1;
            }else{
                rVals->R_rd = 0;
            }
            mips.registers[d->regs.r.rd] = rVals -> R_rd;
            return rVals-> R_rd;
        }else if(funct == 0x08){	//JR PC=R[rs]
            //JR does nothing in ALU, so it does nothing here
            return rVals-> R_rs;
        }else{
            exit(0);
        }
    }else if(d->type == I){ //Instruction I
        int op = d->op;
        if(op == 0x9){ //ADDIU R[rt] = R[rs] + SignExtImm
            rVals->R_rt = rVals->R_rs + d-> regs.i.addr_or_immed;
            mips.registers[d->regs.r.rt] = rVals->R_rt;
            return  rVals-> R_rt;
        }else if(op == 0xc){ //ANDI R[rt] = R[rs] & ZeroExtImm
            rVals->R_rt = ((rVals->R_rs) & (0x0000ffff)) & d-> regs.i.addr_or_immed;
            mips.registers[d->regs.r.rt] = rVals->R_rt;
            return  rVals-> R_rt;
        }else if(op == 0xd){ //ORI R[rt] = R[rs] | ZeroExtImm
            rVals->R_rt = ((rVals->R_rs) & (0x0000ffff)) | d-> regs.i.addr_or_immed;
            mips.registers[d->regs.r.rt] = rVals->R_rt;
            return  rVals-> R_rt;
        }else if(op == 0xf){  //LUI R[rt] = {imm, 16’b0}
            rVals->R_rt = (d-> regs.i.addr_or_immed << 16);
            mips.registers[d->regs.r.rt] = rVals->R_rt;
            return  rVals-> R_rt;
        }else if(op == 0x23){ //LW R[rt] = M[R[rs]+SignExtImm]
            return ((rVals->R_rs + d-> regs.i.addr_or_immed) << 4);
        }else if(op == 0x2b){ //SW M[R[rs]+SignExtImm] = R[rt]
            return ((rVals->R_rs + d-> regs.i.addr_or_immed) << 4);
        }else if(op == 0x4){  //BEQ  if(R[rs]==R[rt])PC=PC+4+BranchAddr
            if(rVals ->R_rs == rVals ->R_rt){
                return 1;
            }else{
                return 0;
            }
        }else if(op == 0x5){ //BNE if(R[rs]!=R[rt]) PC=PC+4+BranchAddr
            if(rVals ->R_rs != rVals ->R_rt){
                return 1;
            }else{
                return 0;
            }
        }else{
            exit(0);
        }
    }else{ //Instruction J
        int op = d->op;
        if(op == 0x2){ //J  PC=JumpAddr
            return 0;//It does nothing in execute stage
        }else if(op == 0x3){ //JAL R[31]=PC+8;PC=JumpAddr
            mips.registers[31]=mips.pc+4;
            return 0;
        }else{
            exit(0);
        }
    }
}

/*
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC(DecodedInstr *d, int val) {
    /* Your code goes here */

    /*For all instructions w/o exceptions, PC+4;*/

    int rs = d->regs.r.rs;
    int aori = d->regs.i.addr_or_immed;
    int target = d->regs.j.target;

    //R-format (jump register exception)
    if (d->type == R) {
        //JR PC=R[rs]
        if (d->regs.r.funct == 0x08) {
            mips.pc = mips.registers[rs];
        } else {
            //PC+4
            mips.pc += 4;
        }

      //I-format (branches exception)
    } else if (d->type == I) {
        //BEQ instruction
        if (d->op == 0x4) {
            if (val == 1) {
                mips.pc = (aori);
            } else {
                mips.pc += 4;
            }
        //BNE instruction
        } else if (d->op == 0x5) {
            if (val == 1) {
                mips.pc = (aori);
            } else {
                mips.pc += 4;
            }
        } else {
            mips.pc += 4;
        }

        //J-format
    } else {
        mips.pc = (((mips.pc + 4) & 0x10000000) + (target));
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value
 * that is read, otherwise return -1.
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1]
 * with address 0x00400004, and so forth.
 *
 */
int Mem(DecodedInstr *d, int val, int *changedMem) {
    /* Your code goes here */

    if(d->op == 0x23 || d->op == 0x2b){
        if(val < 0x00401000 || val > 0x00403fff){
            printf("Memory Access Exception at [0x%x]: address [0x%x]\n",mips.pc-4,val);
            exit(0);
        }else{
            if(d->op == 0x23){        //LW R[rt] = M[R[rs]+SignExtImm]
                val = mips.memory[(val - 0x00400000)/4];
                *changedMem = -1;
                return val;
            }else{   //SW M[R[rs]+SignExtImm] = R[rt]
                mips.memory[(val-0x00400000)/4] = mips.registers[d->regs.i.rt];
                *changedMem = val;
                return -1;
            }
        }
    }else{
        return -1;
    }
}

/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite(DecodedInstr *d, int val, int *changedReg) {
    /* Your code goes here */

    //R-format needs to WB all register values except for j register
    //I-format needs to WB all register values except for sw,beq and bne registers
    //J-format needs to WB only jal register

    int noWB = -1;

    //R-format
    if (d->type == R) {
        //no updates for j register
        if (d->regs.r.funct == 0x08) {
            *changedReg = noWB;
            //else update any other R-format register values
        } else {
            *changedReg = d->regs.r.rd;
        }

        //I-format
    } else if (d->type == I) {
        //update register value for lw
        if (d->op == 0x23) {
            mips.registers[d->regs.r.rt] = val;
            *changedReg = d->regs.r.rt;
            //no updates for sw, beq and bne registers
        } else if (d->op == 0x2b || d->op == 0x4 || d->op == 0x5) {
            *changedReg = noWB;
            //else update register values
        } else {
            *changedReg = d->regs.r.rt;
        }

        //J-format
    } else {
        //update register value to 31 for jal
        if (d->op == 3) {
            *changedReg = 31;
            //else no updates
        } else {
            *changedReg = noWB;
        }
    }
}
