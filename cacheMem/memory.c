#include "tips.h"

/* Define Cache Parameters */
cacheSet cache[MAX_SETS];
unsigned int block_size;
unsigned int set_count;
unsigned int assoc;
ReplacementPolicy policy;
MemorySyncPolicy memory_sync_policy;


void init_memory() 
{
  flush_cache();
}

void flush_cache() 
{
  int set_index;
  int block_index;

  /* for each set */
  for( set_index=0; set_index < set_count; set_index++ )
  {
    /* for each block in the set */
    for( block_index=0; block_index < assoc; block_index++ ) 
    {
      cache[set_index].block[block_index].valid = INVALID;
      cache[set_index].block[block_index].dirty = VIRGIN;
      init_lru(set_index, block_index);
      init_lfu(set_index, block_index);
    }
  }
}

static int translateAddress(address virtual_addr, address* physical_addr)
{
  static struct PageTableEntry {
    word virtual_page_number;
    word physical_page_number;
  } pagetable[PAGE_TABLE_SIZE] =
    { {PROGRAM_START / PHYSICAL_PAGE_SIZE, 0},
      {GLOBAL_START / PHYSICAL_PAGE_SIZE, 1},
      {0x00000000 / PHYSICAL_PAGE_SIZE, 2},
      {STACK_START / PHYSICAL_PAGE_SIZE, 3}
    };

  word i;
  word virtual_page;
  word offset;

  virtual_page = virtual_addr / PHYSICAL_PAGE_SIZE;
  offset = virtual_addr % PHYSICAL_PAGE_SIZE;

  for(i = 0; i < PAGE_TABLE_SIZE; i++)
  {
    if(pagetable[i].virtual_page_number == virtual_page)
    {
      *physical_addr = pagetable[i].physical_page_number * PHYSICAL_PAGE_SIZE + offset;
      return 0;
    }
  }

  append_log("Unable to access memory address\n");
  return -1;
}

int accessDRAM(address addr, byte* data, TransferUnit mode, WriteEnable flag)
{
  static byte DRAM[PHYSICAL_PAGE_COUNT * PHYSICAL_PAGE_SIZE];
  static char* reading = "Accessing";
  static char* writing = "Updating";
#ifdef CYGWIN
  static instruction self_branch = 0xffff0010;
#else
  static instruction self_branch = 0x0100ffff;
#endif
  char buffer[200];
  int transfer_size;
  address phys_addr;
  int error = 0;
  char* memory_action;
  
  /* Determine number of bytes involved in memory access */
  switch(mode)
  {
  case BYTE_SIZE:
    transfer_size = 1;
    break;
  case HALF_WORD_SIZE:
    transfer_size = 2;
    break;
  case WORD_SIZE:
    transfer_size = 4;
    break;
  case DOUBLEWORD_SIZE:
    transfer_size = 8;
    break;
  case QUADWORD_SIZE:
    transfer_size = 16;
    break;
  case OCTWORD_SIZE:
    transfer_size = 32;
    break;
  default:
    append_log("Invalid transfer mode for accessDRAM\nDefaulting to moving only 1 byte");
    error = 1;
  }

  /* Convert virtual address into physical address */
  if(translateAddress(addr, &phys_addr) == -1)
  {    
    if(flag == READ && mode == WORD_SIZE)
      memcpy(data, &self_branch, sizeof(instruction));
    return -1;
  }

  /* Do memory action */
  switch(flag)
  {
  case READ:        
    memcpy(data, DRAM + phys_addr, transfer_size);
    memory_action = reading;
    break;
  case WRITE:
    memcpy(DRAM + phys_addr, data, transfer_size);
    memory_action = writing;
    break;
  default:
    append_log("Invalid flag for accessDRAM\n");
    error = 1;
  }

  /* Announce memory access */
  sprintf(buffer, "%s %u bytes at 0x%08X\n", memory_action, transfer_size, addr);
  if(!IS_GUI_ACTIVE())
    printf(buffer);
  else
    append_log(buffer);

  return error;
}
