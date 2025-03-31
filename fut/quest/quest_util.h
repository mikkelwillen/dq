// Utility file to be included by benchmark file.c in subdirectory...
//

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "QuEST.h"

void usage(char* name) {
  printf("Usage: %s [OPTIONS] N\nOPTIONS:\n\
    -t : print time used\n", name);
  exit(-1);
}

long gettm() {
  struct timeval tc;
  gettimeofday(&tc,NULL);
  return (long)tc.tv_sec * 1000 + (long)tc.tv_usec / 1000;
}

void reporttm(char* s, int N, long tic) {
  long toc = gettm();
  printf("Elapsed time %s %d: %f seconds\n", s, N, (double)(toc-tic)/1000.0);
}

int timing = 0;

int runopts(int argc, char* argv[]) {
  int N = 0;
    int optIdx = 1;
  if (argc <= 1) {
    usage(argv[0]);
  }
  while (optIdx < argc) {
    if (strcmp(argv[optIdx],"-t") == 0) {
      printf("Enabling timing\n");
      timing = 1;
      optIdx++;
      continue;
    }
    if ((N = atoi(argv[optIdx])) > 1) {
      optIdx++;
      break;
    } else {
      printf("**ERR: expecting number\n");
      usage(argv[0]);
    }
  }
  if (N <= 0) {
    printf("Expecting positive integer\n");
    usage(argv[0]);
  }
  return N;
}
