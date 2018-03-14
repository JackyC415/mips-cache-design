#include "tips.h"
#include "util.h"
#include <netinet/in.h>

char* program_name;
CacheView view;
int gui_active;

void validate_cache_parameters(int set_count_value, int assoc_value, int block_size_value)
{
  if(assoc_value < 0)
    assoc = 0;
  else if(assoc_value > MAX_ASSOC)
    assoc = MAX_ASSOC;
  else
    assoc = assoc_value;

  if(set_count_value < 0)
    set_count = 0;
  else if(set_count_value > MAX_SETS)
    set_count = MAX_SETS;
  else if(set_count_value != 0)
    set_count = 1 << uint_log2(set_count_value);
  else
    set_count = 0;

  if(block_size_value < 0)
    block_size = 0;
  else if(block_size_value > MAX_BLOCK_SIZE)
    block_size = MAX_BLOCK_SIZE;
  else if(block_size_value != 0)
  {
    block_size = 1 << uint_log2(block_size_value);    
    if(block_size == 1 || block_size == 2)
      block_size = 4;
  } 
  else
    block_size = 0;
}

int load_dumpfile(const char* filename)
{
  char buffer[200];
  FILE* dumpfile;
  int i;
  byte* inst = (byte*)(malloc(sizeof(byte) * sizeof(instruction)));

  /* Read in file */
  if(!(dumpfile = fopen(filename, "rb")))
  {
    sprintf(buffer, "Unable to load [%s]\n", filename);
    append_log(buffer);
    return -1;
  }
  else
  {
    sprintf(buffer, "[%s] loaded\n", filename);
    append_log(buffer);
  }

  /* Load instructions into memory */
  for(i = 0; i < PHYSICAL_PAGE_SIZE && fread(inst, sizeof(instruction), 1, dumpfile); i += sizeof(instruction))
  {
    /* sprintf(buffer, "%02x , %08x\n", *inst, ntohl(*((word*)inst))); */
    reverse_endianness( (instruction*) inst );
    accessDRAM(PROGRAM_START + i, inst, WORD_SIZE, WRITE);
  }  
  
  /* Insert sentinel instruction */
  *((word*)inst) = 0xffffffff;
  accessDRAM(PROGRAM_START + i, inst, WORD_SIZE, WRITE);

  fclose(dumpfile);
  free(inst);

  /* Initialize processor */
  reinit_processor();
  flush_cache();
  return 0;
}

void reverse_endianness(instruction* word)
{
  int w = 0, i = 0;
  for( i = 0; i < 4; i++ ) {
    w |= ((*word>>(i*8)) & 0xff) << (3-i)*8;
  }
  *word = w;
}

int main(int argc, char** argv)
{
  program_name = argv[0];
  gui_active = 1;

  /* Initialize parameters */
  set_count = 0;
  assoc = 0;
  block_size = 0;
  policy = LRU;
  view = INDEX;
  memory_sync_policy = WRITE_BACK;

  /* Initialize memory */
  init_memory();

  /* Check for flags */
  if(argc >= 2 && (strcmp(argv[1], "-nogui") == 0))
    gui_active = 0;

  /* Build GUI */
  if(IS_GUI_ACTIVE())
    build_gui(argc, argv);
  else    
    activate_no_gui(argc, argv);  
  
  return 0;
}
