#include "tips.h"
#include <signal.h>
#include <ctype.h>
#include <unistd.h>

/******************************************************************************
   String Tokenizer definitions
 *****************************************************************************/

typedef struct _StringTokenizer {
  char *content;
  char *crt;
  char *token;
} StringTokenizer;

StringTokenizer* initTokenizer(char *s) {
  StringTokenizer *sTokenizer;

  sTokenizer = (StringTokenizer*) malloc(sizeof(StringTokenizer));
  sTokenizer->content = s;
  sTokenizer->crt = s;
  sTokenizer->token = (char*) malloc(strlen(s)*sizeof(char));
  return sTokenizer;
}

char* nextToken(StringTokenizer *st) {
  int i = 0;

  while ((st->crt)[i] != '\0' && (st->crt)[i] != '\n' && (st->crt)[i] != ' ') {
    (st->token)[i] = (st->crt)[i];
    i++;
  }  
  (st->token)[i] = '\0';
  while ((st->crt)[i] != '\0' && (st->crt)[i] != '\n' && (st->crt)[i] == ' ') 
    i++;
  st->crt = st->crt + i;
  return st->token;
}

void destroy_tokenizer(StringTokenizer* st)
{
  free(st->token);
  free(st);
}

/******************************************************************************
   No GUI Definition
 *****************************************************************************/

int run_active;

void catch(int sig)
{
  if(sig != SIGINT)
    exit(sig);

  if(run_active == 1)
    run_active = 0;
  else
    exit(sig);
}

void display_regs()
{
  static char* register_display[] = { 
    "R00: %08x\tR08: %08x\tR16: %08x\tR24: %08x\n",
    "R01: %08x\tR09: %08x\tR17: %08x\tR25: %08x\n",
    "R02: %08x\tR10: %08x\tR18: %08x\tR26: %08x\n",
    "R03: %08x\tR11: %08x\tR19: %08x\tR27: %08x\n",
    "R04: %08x\tR12: %08x\tR20: %08x\tR28: %08x\n",
    "R05: %08x\tR13: %08x\tR21: %08x\tR29: %08x\n",
    "R06: %08x\tR14: %08x\tR22: %08x\tR30: %08x\n",
    "R07: %08x\tR15: %08x\tR23: %08x\tR31: %08x\n",
    " PC: %08x\n"
  };

  int i;

  printf("\n");
  for(i = 0; i < 8; i++)
    printf(register_display[i], registers[i], registers[i + 8], registers[i + 16], registers[i+24]);

  /* Display PC */
  printf(register_display[i], PC);
  printf("\n");
}

void display_cache()
{
  int s;
  int b;
  int o;

  /* Setup cache view */
  printf("\n");
  
  if(assoc == 0 || set_count == 0 || block_size == 0)
  {
    printf("Some cache parameters are set to 0\n  + Assoc: %u\n  + Set Count: %u\n  + Block Size: %u\n\n", assoc, set_count, block_size);
    return;
  }

  switch(view)
  {
  case INDEX:
    printf("Set V D  LRU\tLFU\tTag\n=== = =  ===\t===\t===\n");
    for(b = 0; b < set_count; b++)
    {
      for(s = 0; s < assoc; s++)
      {
	printf("%2d  %d %d  %s\t%s\t%08x    ", b, cache[b].block[s].valid, cache[b].block[s].dirty, lru_to_string(b, s), lfu_to_string(b, s), cache[b].block[s].tag);
	for(o = 0; o < block_size; o++)
	{
	  printf("%02x", cache[b].block[s].data[o]);

	  if((o + 1) != block_size)
	  {
	    if((o + 1) % 4 == 0)
	      printf(" | ");
	    else if((o + 1) % 2 == 0)
	      printf(" ");
	  }
	}
	printf("\n");
      }
      printf("\n");
    }
    break;
  case ASSOC:
    for(s = 0; s < assoc; s++)
    {
      printf("Unit #%u\n\nBlk V D  LRU\tLFU\tTag\n=== = =  ===\t===\t===\n", s);

      for(b = 0; b < set_count; b++)
      {
	printf("%2d  %d %d  %s\t%s\t%08x    ", b, cache[b].block[s].valid, cache[b].block[s].dirty, lru_to_string(b, s), lfu_to_string(b, s), cache[b].block[s].tag);
	for(o = 0; o < block_size; o++)
	{
	  printf("%02x", cache[b].block[s].data[o]);

	  if((o + 1) != block_size)
	  {
	    if((o + 1) % 4 == 0)
	      printf(" | ");
	    else if((o + 1) % 2 == 0)
	      printf(" ");
	  }
	}
	printf("\n");
	if((b + 1) % 4 == 0)
	  printf("\n");
      }
    }
    break;
  default:
    printf("Invalid cache arrangement");
    exit(1);
  }

}

void display_help()
{
  printf("\n");
  printf("TIPS is a MIPS R2000 simulator with a cache module by a CSE31 student\n");
  printf("Its top-level commands are:\n");
  printf("\n");
  printf("exit -- Exit the simulator\n");
  printf("\n");
  printf("quit -- Exit the simulator\n");
  printf("\n");
  printf("load <file> -- Load <file> of binary machine code into memory\n");
  printf("\n");
  printf("config <set_count> <assoc> <block_size> <Replacement Policy> <Sync Policy> --\n");
  printf("  Set cache to have <set_count> sets (i.e. number of unique indexes), <assoc>\n");
  printf("  blocks per setm with each block to have size <block_size>. <Replacment\n");
  printf("  Policy> is either 'lru' for LRU, 'r' for RANDOM, or 'lfu' for LFU.\n");
  printf("  <Sync Policy> is either 'wb' for WRITE_BACK or 'wt' for WRITE_THROUGH\n");
  printf("\n");
  printf("view <mode> -- change how cache is drawn. <mode> is either 'index' for\n");
  printf("  index-based view of cache or 'assoc' for associativity-based view\n");
  printf("\n");
  printf("step N -- Step the program for N instructions\n");
  printf("\n");
  printf("run <time>-- Start automated simulation with instructions executing\n");
  printf("  every <time> milliseconds. Press Ctrl-C to stop the simulation\n");
  printf("\n");
  printf("print regs -- Print all MIPS registers\n");
  printf("\n");
  printf("print cache -- Print the current cache state\n");
  printf("\n");
  printf("reset cpu -- Reset the PC and $sp back to startup values\n");
  printf("\n");
  printf("reset cache -- Flush the cache\n");
  printf("\n");
  printf("reinit -- does \"reset cpu\" and \"reset cache\" commands\n");
  printf("\n");
  printf("help -- List top-level commands\n");
}

void configure_cache(StringTokenizer* tokenizer)
{
  int assoc;
  int index;
  int block;
  ReplacementPolicy p;
  MemorySyncPolicy m;
  char* command;

  /* Get index */
  command = nextToken(tokenizer);
  if(strlen(command) != 0)
    index = atoi(command);
  else
  {
    printf("Insufficient arguments\n");
    return;
  }

  /* Get assoc */
  command = nextToken(tokenizer);
  if(strlen(command) != 0)
    assoc = atoi(command);
  else
  {
    printf("Insufficient arguments\n");
    return;
  }

  /* Get block */
  command = nextToken(tokenizer);
  if(strlen(command) != 0)
    block = atoi(command);
  else
  {
    printf("Insufficient arguments\n");
    return;
  }
	
  /* Get replacement policy */
  command = nextToken(tokenizer);
  if(strlen(command) != 0)
  {
    if(strcmp(command, "lru") == 0)
      p = LRU;
    else if(strcmp(command, "r") == 0)
      p = RANDOM;
    else if(strcmp(command, "lfu") == 0)
      p = LFU;
    else
    {
      printf("Invalid parameter for Replacement Policy\n");
      return;
    }
  }
  else
  {
    printf("Insufficient arguments\n");
    return;
  }

  /* Get memory sync policy */
  command = nextToken(tokenizer);
  if(strlen(command) != 0)
  {
    if(strcmp(command, "wb") == 0)
      m = WRITE_BACK;
    else if(strcmp(command, "wt") == 0)
      m = WRITE_THROUGH;
    else
    {
      printf("Invalid parameter for Memory Sync Policy\n");
      return;
    }
  }
  else
  {
    printf("Insufficient arguments\n");
    return;
  }

  validate_cache_parameters(index, assoc, block);      
  policy = p;
  memory_sync_policy = m;

  //I'm probably making this way too ugly with the additional check for LFU
  printf("\nCache parameters changed:\n + set count = %d\n + associativity = %d\n + block size = %d\n + replacement policy = %s\n + memory sync policy = %s\n", set_count, assoc, block_size, (policy == RANDOM ? "Random" : (policy == LRU ? "LRU" : "LFU")), (memory_sync_policy == WRITE_BACK ? "Write Back" : "Write Through"));
}

void do_step(StringTokenizer* tokenizer)
{
  char* command = nextToken(tokenizer);
  int i;
  int n;

  if(strlen(command) == 0)
    n = 1;
  
  n = atoi(command);
  if(n <= 0)
    n = 1;

  for(i = 0; i < n; i++)
    step_processor();
}

void start_simulation(StringTokenizer* tokenizer)
{
  char* command = nextToken(tokenizer);
  int speed;
  if(strlen(command) == 0)
  {
    printf("Please specify a speed.\n");
    return;
  }

  speed = atoi(command);
  if(speed < 1)
    speed = MIN_SPEED;
  else if(speed > MAX_SPEED)
    speed = MAX_SPEED;

  /* Begin execution */

}

void activate_no_gui(int argc, char** argv)
{
  int console_active = 1;
  StringTokenizer* tokenizer;
  char input[200];
  char* command;
  int speed;

  (void)signal(SIGINT, catch);
  run_active = 0;
  
  printf("Tips v2 Started\n");

  /* Load file if any */
  if(argc >= 3)
    load_dumpfile(argv[argc - 1]);

  while(console_active)
  {
    printf("\n[%s] > ", program_name);
    fgets(input, 200, stdin);

    tokenizer = initTokenizer(input);
    command = nextToken(tokenizer);

    if(strcmp(command, "quit") == 0)
      console_active = 0;
    else if(strcmp(command, "exit") == 0)
      console_active = 0;
    else if(strcmp(command, "print") == 0 ||
	    strcmp(command, "display") == 0)
    {
      command = nextToken(tokenizer);
      if(strcmp(command, "regs") == 0)
	display_regs();
      else if(strcmp(command, "cache") == 0)
	display_cache();
      else
	printf("Invalid command: %s\n", input);
    }
    else if(strcmp(command, "config") == 0)
      configure_cache(tokenizer);
    else if(strcmp(command, "view") == 0)
    {
      command = nextToken(tokenizer);
      if(strcmp(command, "i") == 0 || strcmp(command, "index") == 0)
      {
	view = INDEX;
	printf("Cache view now index based\n");
      }
      else if(strcmp(command, "a") == 0 || strcmp(command, "assoc") == 0)
      {
	view = ASSOC;
	printf("Cache view now assoc based\n");
      }
      else
	printf("Invalide command: %s\n", input);
    }
    else if(strcmp(command, "load") == 0)
    {
      command = nextToken(tokenizer);
      load_dumpfile(command);
    }
    else if(strcmp(command, "s") == 0)
      do_step(tokenizer);
    else if(strcmp(command, "step") == 0)
      do_step(tokenizer);
    else if(strcmp(command, "run") == 0)
    {
      command = nextToken(tokenizer);
      speed = atoi(command);
      if(speed < 10)
	speed = 10;
      run_active = 1;
      while(run_active)
      {
	step_processor();
	usleep(1000 * speed);
      }
    }
    else if(strcmp(command, "reinit") == 0)
    {
      reinit_processor();
      printf("\nPC reset");
      flush_cache();
      printf("\nCache flushed\n");
    }
    else if(strcmp(command, "reset") == 0)
    {
      command = nextToken(tokenizer);
      if(strcmp(command, "cpu") == 0)
      {
	reinit_processor();
	printf("\nPC reset\n");
      }
      else if(strcmp(command, "cache") == 0)
      {
	flush_cache();
	printf("\nCache flushed\n");
      }
      else
	printf("Invalid command: %s\n", input);
    }
    else if(strcmp(command, "help") == 0)
      display_help();
    else if(strlen(command) != 0)
      printf("Invalid command: %s\n", input);

    destroy_tokenizer(tokenizer);
  }
}
