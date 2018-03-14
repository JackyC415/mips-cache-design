#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define IS_GUI_ACTIVE() (gui_active == 1)

typedef enum {INDEX, ASSOC} CacheView;
typedef unsigned char byte;
typedef unsigned int word;
typedef unsigned int address;
typedef unsigned int instruction;

/* Define Memory Constants */
#define PHYSICAL_PAGE_SIZE 16384
#define PHYSICAL_PAGE_COUNT 4
#define PAGE_TABLE_SIZE 4

/* Define Address Space Constants */
#define PROGRAM_START 0x00400000
#define PROGRAM_END PROGRAM_START + PHYSICAL_PAGE_SIZE
#define GLOBAL_START 0x10010000
#define STACK_START 0x7fffeffc

/* Define Cache Constants */
#define MAX_BLOCK_SIZE 32
#define MAX_SETS 16
#define MAX_ASSOC 5

/* Define Execution Constants */
#define MIN_SPEED 10
#define MAX_SPEED 2000

/* Variables that will have to be externed */
extern CacheView view;
extern int gui_active;
extern unsigned int registers[32];
extern unsigned int hilo[2];
extern address PC;
extern char* program_name;


/*****************************************************************************
 *
 *     Start reading below this point
 *
 ****************************************************************************/

/*****************************************************************************
  Typedef some useful states for variables
*****************************************************************************/

typedef enum {RANDOM, LRU, LFU} ReplacementPolicy;
typedef enum {WRITE_BACK, WRITE_THROUGH} MemorySyncPolicy;
typedef enum {READ, WRITE} WriteEnable;
typedef enum {BYTE_SIZE = 0, HALF_WORD_SIZE, WORD_SIZE, DOUBLEWORD_SIZE, QUADWORD_SIZE, OCTWORD_SIZE} TransferUnit;
typedef enum {HIT, MISS} CacheAction;

/*****************************************************************************
  Define cache variables and memory structure and functions 
*****************************************************************************/

extern unsigned int set_count;               /* Number of sets            */
extern unsigned int assoc;                   /* Cache associativity       */
extern unsigned int block_size;              /* Cache block size in bytes */
extern ReplacementPolicy policy;             /* Cache replacement policy  */
extern MemorySyncPolicy memory_sync_policy;  /* Memory sync policy        */

/* Define cache block
   ==================
   valid - assign INVALID if block invalid; assign VALID if block valid
   tag - container for the tag bits; unsigned to allow ignoring sign ext issue
   data - the data contained in a block
   lru.data - pointer to lru information
   lru.value - int that represents lru information
*/
typedef struct {
  enum {INVALID, VALID} valid;   
  enum {VIRGIN, DIRTY} dirty;
  unsigned int tag;
  byte data[MAX_BLOCK_SIZE];
  union { 
    void* data;
    unsigned int value;
  } lru;
  int accessCount;
} cacheBlock;

/* Define cache unit
   =================
   block - array that represents a set of blocks with the SAME index
*/
typedef struct {
  cacheBlock block[MAX_ASSOC];
} cacheSet;

/* Define actual cache structure that will be manipulated by accessMemory() */
extern cacheSet cache[MAX_SETS];

/*
  This function should be called when you want to interact with physical memory

    addr - a 32-bit address of what part of memory that needs to be accessed
    data - pointer to the array used to send data to or from memory
    mode - states the amount of data to transfer using the TransferUnit
           enum variables
    flag - states whether we want to READ from memory or WRITE to memory

  returns 0 if successful, non-zero if there was a problem.

 */
int accessDRAM(address addr, byte* data, TransferUnit mode, WriteEnable flag);


/*
  This function is the function you will be implementing. Its purpose
  is to behave like a cache, using the variables defined earlier. The 
  simulated CPU will call this function when it wants to access memory

    addr - a 32-bit address of what part of memory that needs to be accessed
    data - pointer to the 32-bit container used to send data to or from memory
    flag - states whether we want to READ from memory or WRITE to memory    
*/
void accessMemory(address addr, word* data, WriteEnable flag);

/*
  These are the GUI functions you can call to visualize changes in the cache
 */


/*
  Prints a message into "Execution Log" when running the GUI, or to the
  screen when TIPS is running in "No GUI" mode. You may find it helpful
  to use sprintf in combination with this function to print out variable
  values.

   msg - The string to print out.

 */
void append_log(char* msg);

/*
  Draws a rectangle around the block

    set_num - the set that contains the block to highlight
    assoc_num - the specific block in a set to highlight

 */
void highlight_block(unsigned int set_num, unsigned int assoc_num);

/*
  Highlights an offset in the block

    set_num - the set that contains the block to highlight
    assoc_num - the specific block in a set to highlight
    offset - the specific offset to hightlight
    action - specifies whether the offset was accessed after a MISS or a HIT
    
 */
void highlight_offset(unsigned int set_num, unsigned int assoc_num, unsigned int offset, CacheAction action);

/*****************************************************************************
 *
 *     Stop Reading -- you don't need anything past this point.
 *
 ****************************************************************************/


/*****************************************************************************
  Define function prototypes
*****************************************************************************/

/* Defined in tips.c */
int load_dumpfile(const char* filename);
void reverse_endianness(instruction* word);

/* Defined in memory.c */
void init_memory(void);
void flush_cache(void);

/* Defined in cpu.c */
void reinit_processor(void);
void step_processor(void);

/* Defined in gui.c */
int build_gui(int argc, char** argv);
void refresh_register_display();
void refresh_cache_display();
void stop_run();
void flush_drawlist();

/* Defined in nogui.c */
void activate_no_gui(int argc, char** argv);

/* Defined in cachelogic.c */
void init_lfu(int set_number, int assoc_value);
void init_lru(int set_number, int assoc_value);
char* lfu_to_string(int set_number, int assoc_value);
char* lru_to_string(int set_number, int assoc_value);
void validate_cache_parameters(int set_number, int assoc_value, int block_size_value);
