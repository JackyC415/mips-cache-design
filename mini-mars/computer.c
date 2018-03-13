#include <stdio.h>
#include <stdlib.h>
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

/*Collaborators: Jacky Z. Chen and Isaac Velasquez*/

/*All implementation referenced MIPS Green Sheet -- https://inst.eecs.berkeley.edu/~cs61c/resources/MIPS_Green_Sheet.pdf*/

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
        d->type = 0;

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

        //J-Format; opcode w/ # 2 or 3
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

          //check for branches (beq or bne)
        } else if (d->op == 0x4 || d->op == 0x5) {
            //PC=PC+4+BranchAddr
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

    //R-format instructions
    if(d->type == 0){

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
            printf("jr\t$%d\n",rs);
    }
    //I-format instructions
    else if (d->type == I){

        int rs =d->regs.i.rs;
        int rt=d->regs.i.rt;
        int aori = d->regs.i.addr_or_immed;

        /*printing instructions addiu,andi,ori,lui,beq,bne,lw & sw respectively*/
        if(d->op==0x9)
            printf("addiu\t$%d, $%d, %d\n",rt, rs, aori);
        else if (d->op==0xc)
            printf("andi\t$%d, $%d, %d\n",rt, rs, aori);
        else if (d->op==0xd)
            printf("ori\t$%d, $%d, %d\n",rt, rs, aori);
        else if (d->op==0xf)
            printf("lui\t$%d, $%d\n", rt, aori);
        else if (d->op==0x4)
            printf("beq\t$%d, $%d, 0x%08X\n",rs, rt, aori);
        else if (d->op==0x5)
            printf("bne\t$%d, $%d, 0x%08X\n",rs, rt ,aori);
        else if (d->op==0x23)
            printf("lw\t$%d, %d($%d)\n",rt, aori, rs);
        else if (d->op==0x2b)
            printf("sw\t$%d, %d($%d)\n",rt, aori, rs);
    }

    //J-format instructions
    else if (d->type == J){

        int target = d->regs.j.target;

        /*printing instructions j and jal respectively*/
        if (d->op==0x2)
            printf("j\t 0x%08X\n",target);
        else if (d->op==0x3)
            printf("jal\t 0x%08X\n",target);
    }

}

/* Perform computation needed to execute d, returning computed value */
int Execute(DecodedInstr *d, RegVals *rVals) {
    /* Your code goes here */

    //R-format instructions
    if(d->type == 0){

        int funct = d->regs.r.funct;
        int rd = d->regs.r.rd;
        int shamt = d-> regs.r.shamt;

        //addu: R[rd] = R[rs] + R[rt]
        if(funct == 0x21){
            rVals->R_rd = rVals->R_rs + rVals-> R_rt;
            mips.registers[rd] = rVals -> R_rd;
            return  rVals-> R_rd;
            //subu: R[rd] = R[rs] - R[rt]
        }else if(funct == 0x23){
            rVals->R_rd = rVals->R_rs - rVals-> R_rt;
            mips.registers[rd] = rVals -> R_rd;
            return  rVals-> R_rd;
            //sll: R[rd] = R[rt] << shamt
        }else if(funct == 0x00){
            rVals->R_rd = rVals-> R_rt << shamt;
            mips.registers[rd] = rVals -> R_rd;
            return  rVals-> R_rd;
            //srl: R[rd] = R[rt] >> shamt
        }else if(funct == 0x02){
            rVals->R_rd = (rVals-> R_rt >> shamt) & ~(((0x1 << 31) >> shamt) << 1);
            mips.registers[rd] = rVals -> R_rd;
            return  rVals-> R_rd;
            //and: R[rd] = R[rs] & R[rt]
        }else if(funct == 0x24){
            rVals-> R_rd = rVals-> R_rt & rVals->R_rs;
            mips.registers[rd] = rVals -> R_rd;
            return rVals-> R_rd;
            //or: R[rd] = R[rs] | R[rt]
        }else if(funct == 0x25){
            rVals-> R_rd = rVals-> R_rt | rVals->R_rs;
            mips.registers[rd] = rVals -> R_rd;
            return rVals-> R_rd;
            //slt: R[rd] = (R[rs] < R[rt]) ? 1 : 0
        }else if(funct == 0x2a){
            if(rVals-> R_rs < rVals-> R_rt){
                rVals->R_rd = 1;
            }else{
                rVals->R_rd = 0;
            }
            mips.registers[rd] = rVals -> R_rd;
            return rVals-> R_rd;
            //jr: PC=R[rs]
        }else if(funct == 0x08){
            return rVals-> R_rs;
        }else{
            exit(0);
        }

    //I-format instructions
    }else if(d->type == I){

        int rt = d->regs.r.rt;
        int aori = d-> regs.i.addr_or_immed;

        //addiu: R[rt] = R[rs] + SignExtImm
        if(d->op == 0x9){
            rVals->R_rt = rVals->R_rs + aori;
            mips.registers[rt] = rVals->R_rt;
            return  rVals-> R_rt;
            //andi: R[rt] = R[rs] & ZeroExtImm
        }else if(d->op == 0xc){
            rVals->R_rt = ((rVals->R_rs) & (0x0000ffff)) & aori;
            mips.registers[rt] = rVals->R_rt;
            return  rVals-> R_rt;
            //ori: R[rt] = R[rs] | ZeroExtImm
        }else if(d->op == 0xd){
            rVals->R_rt = ((rVals->R_rs) & (0x0000ffff)) | aori;
            mips.registers[rt] = rVals->R_rt;
            return  rVals-> R_rt;
            //lui: R[rt] = {imm, 16’b0}
        }else if(d->op == 0xf){
            rVals->R_rt = (aori<< 16);
            mips.registers[rt] = rVals->R_rt;
            return  rVals-> R_rt;
            //lw: R[rt] = M[R[rs]+SignExtImm]
        }else if(d->op == 0x23){
            return ((rVals->R_rs + aori) << 4);
            //sw: M[R[rs]+SignExtImm] = R[rt]
        }else if(d->op == 0x2b){
            return ((rVals->R_rs + aori) << 4);
            //beq:  if(R[rs]==R[rt])PC=PC+4+BranchAddr
        }else if(d->op == 0x4){
            if(rVals ->R_rs == rVals ->R_rt){
                return 1;
            }else{
                return 0;
            }
            //bne: if(R[rs]!=R[rt]) PC=PC+4+BranchAddr
        }else if(d->op == 0x5){
            if(rVals ->R_rs != rVals ->R_rt){
                return 1;
            }else{
                return 0;
            }
        }else{
            exit(0);
        }

    //J-format instructions
    }else{
        //j: PC=JumpAddr
        if(d->op == 0x2){
            return 0;
            //jal: R[31]=PC+8;PC=JumpAddr
        }else if(d->op == 0x3){
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

    //R-format instructions
    if (d->type == 0) {
        //jr exception
        if (d->regs.r.funct == 0x08) {
            mips.pc = mips.registers[d->regs.r.rs];
        } else {
            mips.pc += 4;
        }

        //I-format instructions
    } else if (d->type == I) {
        //beq exception
        if (d->op == 0x4) {
            if (val == 1) {
                mips.pc = (d->regs.i.addr_or_immed);
            } else {
                mips.pc += 4;
            }
            //bne exception
        } else if (d->op == 0x5) {
            if (val == 1) {
                mips.pc = (d->regs.i.addr_or_immed);
            } else {
                mips.pc += 4;
            }
        } else {
            mips.pc += 4;
        }

        //J-format instructions
    } else {
        mips.pc = (((mips.pc + 4) & 0x10000000) + (d->regs.j.target));
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

	/*SW will be accessing & updating memory due to storage, but will not WB to register; whereas,
	LW will WB to register since we're extracting the data, but will not be updated in memory*/
	
    //instruction sw or lw only
    if(d->op == 0x2b || d->op == 0x23){
	   
	//invalid memory address
        if(val > 0x00403fff || val < 0x00401000){
	/*"Memory Access Exception at [PC val]: address [address]", where [PC val] is the current PC, 
	and [address] is the offending address, both printed in hex (with leading 0x). 
	Then you must call exit(0)*/
            printf("Memory Access Exception at [0x%x]: address [0x%x]\n",mips.pc-4,val);
            exit(0);
        
	} else{
            //sw: M[R[rs]+SignExtImm] = R[rt]
            if(d->op == 0x2b){
                mips.memory[(val-0x00400000)/4] = mips.registers[d->regs.i.rt];
                *changedMem = val;
                return -1;
            }else{
                //lw: R[rt] = M[R[rs]+SignExtImm]
                val = mips.memory[(val - 0x00400000)/4];
                //no updates
                *changedMem = -1;
                return val;
            }
        }
    }
    return -1;
}

/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite(DecodedInstr *d, int val, int *changedReg) {
    /* Your code goes here */

    /*R-format needs to WB all register values except for j register
    I-format needs to WB all register values except for sw,beq and bne registers
    J-format needs to WB only jal register*/

    int noWB = -1;

    //R-format
    if (d->type == 0) {
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
            //else update any other I-format register values
        } else {
            *changedReg = d->regs.r.rt;
        }

      //J-format
    } else {
        //update only register value to 31 for jal
        if (d->op == 3) {
            *changedReg = 31;
            //else no updates
        } else {
            *changedReg = noWB;
        }
    }
}
