#include "tips.h"

/* finds the highest 1 bit, and returns its position, else 0xffffffff */
unsigned int uint_log2(unsigned int w) 
{ 
  unsigned int x = 0x80000000;
  unsigned int z = 31;
  while(x)
  {
    if(w & x)
      break;

    x = x >> 1;
    z--;
  }

  return z;
}

/* return random int from 0..x-1 */
int randomint( int x ) { 
  return rand()%x;
}
