#include "tips.h"

/* The following two functions are defined in util.c */

/* finds the highest 1 bit, and returns its position, else 0xFFFFFFFF */
unsigned int uint_log2(word w);

/* return random int from 0..x-1 */
int randomint( int x );

/*
  This function allows the lfu information to be displayed

    assoc_index - the cache unit that contains the block to be modified
    block_index - the index of the block to be modified

  returns a string representation of the lfu information
 */
char* lfu_to_string(int assoc_index, int block_index)
{
	/* Buffer to print lfu information -- increase size as needed. */
	static char buffer[9];
	sprintf(buffer, "%u", cache[assoc_index].block[block_index].accessCount);

	return buffer;
}

/*
  This function allows the lru information to be displayed

    assoc_index - the cache unit that contains the block to be modified
    block_index - the index of the block to be modified

  returns a string representation of the lru information
 */
char* lru_to_string(int assoc_index, int block_index)
{
	/* Buffer to print lru information -- increase size as needed. */
	static char buffer[9];
	sprintf(buffer, "%u", cache[assoc_index].block[block_index].lru.value);

	return buffer;
}

/*
  This function initializes the lfu information

    assoc_index - the cache unit that contains the block to be modified
    block_number - the index of the block to be modified

 */
void init_lfu(int assoc_index, int block_index)
{
	cache[assoc_index].block[block_index].accessCount = 0;
}

/*
  This function initializes the lru information

    assoc_index - the cache unit that contains the block to be modified
    block_number - the index of the block to be modified

 */
void init_lru(int assoc_index, int block_index)
{
	cache[assoc_index].block[block_index].lru.value = 0;
}

/*
  This is the primary function you are filling out,
  You are free to add helper functions if you need them

  @param addr 32-bit byte address
  @param data a pointer to a SINGLE word (32-bits of data)
  @param we   if we == READ, then data used to return
              information back to CPU

              if we == WRITE, then data used to
              update Cache/DRAM
 */
void accessMemory(address addr, word* data, WriteEnable we)
{
	/* Declare variables here */
	unsigned int tag_length, index_length, offset_length, offset_value, index_value, tag_value;
	unsigned int bHit = 0, LRU_index = 0, LRU_value = 0;
	address addr = 0;
	TransferUnit byte_size = 0;

	/* handle the case of no cache at all - leave this in */
	if(assoc == 0) {
		accessDRAM(addr, (byte*)data, WORD_SIZE, we);
		return;
	}

	/*
  You need to read/write between memory (via the accessDRAM() function) and
  the cache (via the cache[] global structure defined in tips.h)

  Remember to read tips.h for all the global variables that tell you the
  cache parameters

  The same code should handle random, LFU, and LRU policies. Test the policy
  variable (see tips.h) to decide which policy to execute. The LRU policy
  should be written such that no two blocks (when their valid bit is VALID)
  will ever be a candidate for replacement. In the case of a tie in the
  least number of accesses for LFU, you use the LRU information to determine
  which block to replace.

  Your cache should be able to support write-through mode (any writes to
  the cache get immediately copied to main memory also) and write-back mode
  (and writes to the cache only gets copied to main memory when the block
  is kicked out of the cache.

  Also, cache should do allocate-on-write. This means, a write operation
  will bring in an entire block if the block is not already in the cache.

  To properly work with the GUI, the code needs to tell the GUI code
  when to redraw and when to flash things. Descriptions of the animation
  functions can be found in tips.h
	 */

	/* Start adding code here */

  //index is # of sets
	index_length = uint_log2(set_count);
  //offset is block size
	offset_length = uint_log2(block_size);
  //tag is 32 bits minus index and offset
	tag_length = 32 - (index_length + offset_length);
	offset_value = (addr) & ((1 << offset_length) - 1);
	index_value = (addr >> offset_length) & ( (1 << index_length) - 1);
	tag_value = addr >> (offset_length + index_length);

        //user manual block size inputs
	switch (block_size) {
	case(4): byte_size = 2; break;
	case(8): byte_size = 3; break;
	case(16): byte_size = 4; break;
	case(32): byte_size = 5; break;
	}

	if (we == READ)
	{
		bHit = 0;
		int i = 0;
		while(i < assoc) {
      //block hit
			if (tag_value == cache[index_value].block[i].tag && cache[index_value].block[i].valid == 1)
			{
				cache[index_value].block[i].lru.value = 0;
				cache[index_value].block[i].valid = 1;
				bHit = 1;
				memcpy (data,(cache[index_value].block[i].data + offset_value), 4);
			}
			i++;
		}

		if (bHit == 0)
		{
			// least recently used cache replacement policy
			if (policy == LRU)
			{
				int i = 0;
				while(i < assoc) {
					if(LRU_value < cache[index_value].block[i].lru.value)
					{
						LRU_index = i;
						LRU_value = cache[index_value].block[i].lru.value;
					}
					i++;
				}
			}

	//memory access read case
	if (policy == LRU)
	{
		int i = 0;
		while( i < assoc) {
			cache[index_value].block[i].lru.value++;
			i++;
		}
	}
			//random cache replacement policy
			else if (policy == RANDOM)
				LRU_index = randomint(assoc);

			if(cache[index_value].block[LRU_index].dirty == DIRTY)
			{
				addr = cache[index_value].block[LRU_index].tag << (index_length + offset_length) + (index_value << offset_length);
				accessDRAM(Addr, (cache[index_value].block[LRU_index].data), byte_size, WRITE);
			}

			//cache miss occured, access disk memory
			accessDRAM(addr, (cache[index_value].block[LRU_index].data), byte_size, READ);
			cache[index_value].block[LRU_index].lru.value = 0;
			cache[index_value].block[LRU_index].valid = 1;
			cache[index_value].block[LRU_index].dirty = VIRGIN;
			cache[index_value].block[LRU_index].tag = tag_value;

			memcpy (data,(cache[index_value].block[LRU_index].data + offset_value), 4);
		}

	}

	// memory access write case
	else
	{
		bHit = 0;
		if (memory_sync_policy == WRITE_BACK)
		{
			for (int i = 0; i < assoc; i++)
			{
				//block hit
				if (tag_value == cache[index_value].block[i].tag && cache[index_value].block[i].valid == 1)
				{
					memcpy ((cache[index_value].block[i].data + offset_value),data, 4);
					cache[index_value].block[i].dirty = DIRTY;
					cache[index_value].block[i].lru.value = 0;
					cache[index_value].block[i].valid = 1;
					bHit = 1;
				}

			}

			if (bHit == 0)
			{
				// least recently used cache replacement policy
				if (policy == LRU)
				{
					for (int i = 0; i < assoc; i++)
						if(LRU_value < cache[index_value].block[i].lru.value)
						{
							LRU_index = i;
							LRU_value = cache[index_value].block[i].lru.value;
						}
				}
				else if (policy == RANDOM)
					LRU_index = randomint(assoc);

				if(cache[index_value].block[LRU_index].dirty == DIRTY)
				{
					addr = cache[index_value].block[LRU_index].tag << (index_length + offset_length) + (index_value << offset_length);
					//cache miss occured, access DRAM for WRITE
					accessDRAM(Addr, (cache[index_value].block[LRU_index].data), byte_size, WRITE);
				}
				cache[index_value].block[LRU_index].lru.value = 0;
				cache[index_value].block[LRU_index].valid = 1;
				cache[index_value].block[LRU_index].dirty = VIRGIN;
				cache[index_value].block[LRU_index].tag = tag_value;

				//cache miss occured, access DRAM for READ
				accessDRAM(addr, (cache[index_value].block[LRU_index].data), byte_size, READ);
				memcpy ((cache[index_value].block[LRU_index].data + offset_value),data, 4);
			}
		}
    //write through
		else 
		{
			int i = 0;
			while(i < assoc)
			{
        //if block hit
				if (tag_value == cache[index_value].block[i].tag && cache[index_value].block[i].valid == 1) 
				{
					memcpy ((cache[index_value].block[i].data + offset_value),data, 4);
          //update cache index and block values
					cache[index_value].block[i].dirty = VIRGIN;
					cache[index_value].block[i].lru.value = 0;
					cache[index_value].block[i].valid = 1;
					bHit = 1;
					//cache miss occured, access DRAM for WRITE
					accessDRAM(addr, (cache[index_value].block[LRU_index].data), byte_size, WRITE);
				}
				i++;
			}

			if (bHit == 0)
			{
				// check least recently used cache replacement policy
				if (policy == LRU)
				{
					int i = 0;
					while(i < assoc)
						if(LRU_value < cache[index_value].block[i].lru.value)
						{
							LRU_index = i;
							LRU_value = cache[index_value].block[i].lru.value;
						}
				}
				else if (policy == RANDOM)
					LRU_index = randomint(assoc);
				i++;
        memcpy ((cache[index_value].block[LRU_index].data + offset_value),data, 4);
        //update cache index and block values
				cache[index_value].block[LRU_index].lru.value = 0;
				cache[index_value].block[LRU_index].valid = 1;
				cache[index_value].block[LRU_index].dirty = VIRGIN;
				cache[index_value].block[LRU_index].tag = tag_value;
				//cache miss occured, access disk memory for READ
				accessDRAM(addr, (cache[index_value].block[LRU_index].data), byte_size, READ);
			}
		}
	}
}
