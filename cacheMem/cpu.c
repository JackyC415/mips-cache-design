#include "tips.h"
#include <netinet/in.h>

word registers[32];
word hilo[2];
address PC;

/******************************************************************************
   Nice Macros to simplify typing
 *****************************************************************************/

#define rs (registers[getRs(inst)])
#define rt (registers[getRt(inst)])
#define rd (registers[getRd(inst)])
#define jtarget ( (PC & 0xf0000000) | (getTarget(inst) << 2) )
#define btarget (PC + (getSImmed(inst) << 2))
#define hi (hilo[0])
#define lo (hilo[1])

unsigned int getOpcode(const word instr){
  return instr >> 26;
}

unsigned int getFunct(const word instr){
  return instr & 0x3f;
}

unsigned int getRs(const word instr){
  return instr >> 21 & 0x1f;
}

unsigned int getRt(const word instr){
  return instr >> 16 & 0x1f;
}

unsigned int getRd(const word instr){
  return instr >> 11 & 0x1f;
}

unsigned int getShamt(const word instr){
  return instr >> 6 & 0x1f;
}

unsigned int getUImmed(const word instr){
  return instr & 0x0000ffff;
}

int getSImmed(const word instr){
  return instr & 0x8000 ? instr | ~0xffff : instr & 0xffff;
}

unsigned int getTarget(const word instr){
  return instr & 0x03ffffff;
}

void disassemble_inst(word inst)
{
  char buffer[200];
  switch(getOpcode(inst))
  {
  case 0: /* R-type */
    switch(getFunct(inst))
    {
    case 0: /* sll */
      sprintf(buffer, "sll\t\t$%u, $%u, %u\n", getRd(inst), getRt(inst), getShamt(inst));
      break;
    case 2: /* srl */
      sprintf(buffer, "srl\t\t$%u, $%u, %u\n", getRd(inst), getRt(inst), getShamt(inst));
      break;
    case 3: /* sra */
      sprintf(buffer, "sra\t\t$%u, $%u, $%u\n", getRd(inst), getRt(inst), getRs(inst));
      break;
    case 4: /* sllv */ 
      sprintf(buffer, "sllv\t$%u, $%u, $%u\n", getRd(inst), getRt(inst), getRs(inst));
      break;
    case 6: /* srlv */
      sprintf(buffer, "srlv\t$%u, $%u, $%u\n", getRd(inst), getRt(inst), getRs(inst));
      break;
    case 7: /* srav */
      sprintf(buffer, "srav\t$%u, $%u, $%u\n", getRd(inst), getRt(inst), getRs(inst));
      break;
    case 8: /* jr   */
      sprintf(buffer, "jr\t\t$%u\n", getRs(inst));
      break;
    case 9: /* jalr */
      sprintf(buffer, "jalr\t$%u, $%u\n", getRs(inst), getRd(inst));
      break;
    case 16: /* mfhi  */
      sprintf(buffer, "mfhi\t$%u", getRd(inst));
      break;
    case 17: /* mflo  */
      sprintf(buffer, "mflo\t$%u", getRd(inst));
      break;
    case 18: /* mthi  */
      sprintf(buffer, "mtlo\t$%u", getRs(inst));
      break;
    case 19: /* mtlo  */
      sprintf(buffer, "mtlo\t$%u", getRs(inst));
      break;
    case 24: /* mult  */
      sprintf(buffer, "mult\t$%u, $%u\n", getRs(inst), getRt(inst));
      break;
    case 25: /* multu */
      sprintf(buffer, "multu\t$%u, $%u\n", getRs(inst), getRt(inst));
      break;
    case 26: /* div  */
      sprintf(buffer, "div\t$%u, $%u\n", getRs(inst), getRt(inst));
      break;
    case 27: /* divu */
      sprintf(buffer, "divu\t$%u, $%u\n", getRs(inst), getRt(inst));
      break;
    case 32: /* add  */
      sprintf(buffer, "add\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 33: /* addu */
      sprintf(buffer, "addu\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 34: /* sub  */
      sprintf(buffer, "sub\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 35: /* subu */
      sprintf(buffer, "subu\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 36: /* and  */
      sprintf(buffer, "and\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 37: /* or   */
      sprintf(buffer, "or\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 38: /* xor  */
      sprintf(buffer, "xor\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));
      break;
    case 42: /* slt  */
      sprintf(buffer, "slt\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));      
      break;
    case 43: /* sltu */
      sprintf(buffer, "sltu\t$%u, $%u, $%u\n", getRd(inst), getRs(inst), getRt(inst));      
      break;
    default:
      sprintf(buffer, "Unsupported instruction\n");
      break;
    }
    break;
  case 2: /* j     */
    sprintf(buffer, "j\t\t0x%.8X\n", (unsigned int)((PC & 0xf0000000) | getTarget(inst) << 2));
    break;
  case 3: /* jal   */
    sprintf(buffer, "jal\t0x%.8X\n", (unsigned int)((PC & 0xf0000000) | getTarget(inst) << 2));
    break;
  case 4: /* beq   */
    sprintf(buffer, "beq\t$%u, $%u, 0x%.8X\n", getRs(inst), getRt(inst), (unsigned int)((getSImmed(inst) << 2) + PC));
    break;
  case 5: /* bne   */
    sprintf(buffer, "bne\t$%u, $%u, 0x%.8X\n", getRs(inst), getRt(inst), (unsigned int)((getSImmed(inst) << 2) + PC));
    break;
  case 8: /* addi  */
    sprintf(buffer, "addi\t$%u, $%u, %d\n", getRt(inst), getRs(inst), getSImmed(inst));
    break;
  case 9: /* addiu */
    sprintf(buffer, "addiu\t$%u, $%u, %d\n", getRt(inst), getRs(inst), getSImmed(inst));
    break;
  case 10: /* slti  */
    sprintf(buffer, "slti\t$%u, $%u, %d\n", getRt(inst), getRs(inst), getSImmed(inst));
    break;
  case 11: /* sltiu */
    sprintf(buffer, "sltiu\t$%u, $%u, %d\n", getRt(inst), getRs(inst), getSImmed(inst));
    break;
  case 12: /* andi  */
    sprintf(buffer, "andi\t$%u, $%u, 0x%x\n", getRt(inst), getRs(inst), getUImmed(inst));
    break;
  case 13: /* ori   */
    sprintf(buffer, "ori\t$%u, $%u, 0x%x\n", getRt(inst), getRs(inst), getUImmed(inst));
    break;
  case 15: /* lui   */
    sprintf(buffer, "lui\t$%u, 0x%x\n", getRt(inst), getUImmed(inst));
    break;
  case 32: /* lb */
    sprintf(buffer, "Unsupported instruction, lb\n");
    break;
  case 36: /* lbu */
    sprintf(buffer, "Unsupported instruction, lbu\n");
    break;
  case 35: /* lw    */
    sprintf(buffer, "lw\t$%u, %d($%u)\n", getRt(inst), getSImmed(inst), getRs(inst));
    break;
  case 40: /* sb */
    sprintf(buffer, "Unsupported instruction, sb\n");
    break;
  case 43: /* sw    */
    sprintf(buffer, "sw\t$%u, %d($%u)\n", getRt(inst), getSImmed(inst), getRs(inst));
    break;
  case 63: /* sentinel */
    sprintf(buffer, "Done\n");
    break;
  default:
    sprintf(buffer, "Unsupported instruction\n");
  }

  append_log(buffer);
}

void execute_inst(word inst)
{
  char buffer[200];

  switch(getOpcode(inst))
  {
  case 0: /* R-type */
    switch(getFunct(inst))
    {
    case 0: /* sll */
      rd = rt << getShamt(inst);
      break;
    case 2: /* srl */
      rd = rt >> getShamt(inst);
      break;
    case 3: /* sra */
      rd = (int)(rt) >> rs;
      break;
    case 4: /* sllv */ 
      rd = rt << rs;
      break;
    case 6: /* srlv */
      rd = rt >> rs;
      break;
    case 7: /* srav */
      rd = (int)(rt) >> rs;
      break;
    case 8: /* jr   */
      PC = rs;
      break;
    case 9: /* jalr */
      rd = PC;
      PC = rs;
      break;
    case 16: /* mfhi  */
      rd = hi;
      break;
    case 17: /* mflo  */
      rd = lo;
      break;
    case 18: /* mthi  */
      hi = rs;
      break;
    case 19: /* mtlo  */
      lo = rs;
      break;
    case 24: /* mult  */      
    case 25: /* multu */
      lo = rs * rt;
      break;
    case 26: /* div   */
    case 27: /* divu  */
      lo = rs / rt;
      hi = rs % rt;
      break;
    case 32: /* add   */
    case 33: /* addu  */
      rd = rs + rt;
      break;
    case 34: /* sub   */
    case 35: /* subu  */
      rd = rs - rt;
      break;
    case 36: /* and   */
      rd = rs & rt;
      break;
    case 37: /* or    */
      rd = rs | rt;
      break;
    case 38: /* xor   */
      rd = rs ^ rt;
      break;
    case 42: /* slt   */
    case 43: /* sltu  */
      rd = (rs & 0x80000000) ^ (rt & 0x80000000) ? rs >> 31 : rs < rt;
      break;
    default:
      sprintf(buffer, "Unsupported instruction\n");
      break;
    }
    break;
  case 2: /* j     */
    PC = jtarget;
    break;
  case 3: /* jal   */
    registers[31] = PC;
    PC = jtarget;
    break;
  case 4: /* beq   */
    if(rs == rt)
      PC = btarget;
    break;
  case 5: /* bne */
    if(rs != rt)
      PC = btarget;
    break;
  case 8: /* addi */
  case 9: /* addiu */
    rt = rs + getSImmed(inst);
    break;
  case 10: /* slti  */
  case 11: /* sltiu */
    rt = (rs & 0x80000000) ^ (getSImmed(inst) & 0x80000000) ? rs >> 31 : rs < getSImmed(inst);
    break;
  case 12: /* andi  */
    rt = rs & getUImmed(inst);
    break;
  case 13: /* ori */
    rt = rs | getUImmed(inst);
    break;
  case 15: /* lui */
    rt = getUImmed(inst) << 16;
    break;
  case 32: /* lb */
    sprintf(buffer, "Unsupported instruction, lb\n");
    break;
  case 36: /* lbu */
    sprintf(buffer, "Unsupported instruction, lbu\n");
    break;
  case 35: /* lw */
    accessMemory(rs + getSImmed(inst), &rt, READ);
    break;
  case 40: /* sb */
    sprintf(buffer, "Unsupported instruction, sb\n");
    break;
  case 43: /* sw */
    accessMemory(rs + getSImmed(inst), &rt, WRITE);
    break;
  case 63:
    stop_run();
    break;
  default:
    sprintf(buffer, "Unsupported instruction\n");
  }

  /* Ensure $zero remains equal to 0 */
  registers[0] = 0;
}

void reinit_processor()
{
  PC = PROGRAM_START;
  registers[29] = STACK_START;
  registers[31] = PROGRAM_START;
  refresh_register_display();
}

void step_processor()
{
  char buffer[200];
  word inst;

  /* Flush previously drawn items */
  flush_drawlist();

  /* Fetch Instruction */
  accessMemory(PC, &inst, READ);
  inst = ntohl(inst);

  /* Print PC */
  sprintf(buffer, "[0x%08X]: 0x%08X\t", PC, inst);
  append_log(buffer);

  /* Increment PC */
  PC += sizeof(instruction); 

  /* Disassemble Instruction */
  disassemble_inst(inst);

  /* Execute Instruction */
  execute_inst(inst);
  
  /* refresh registers and cache */
  refresh_register_display();
  refresh_cache_display();
}
