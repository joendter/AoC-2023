#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#define time 59688274
#define distance 543102016641022

int main(){
  uint64_t accumulator = 0;
  for (uint64_t t = 1; t < time; t++){
	accumulator += t*(time-t) > distance;
  }
  printf("%llu\n", accumulator);
  return 0;
}


