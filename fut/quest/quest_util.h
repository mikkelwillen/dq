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
    -t : print time used\n\
    -l : output qsim-instructions on <stderr>\n", name);
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

int lqsim = 0;
int timing = 0;

#define tmloop(s, N, f) {                                       \
    int ms_min = 1000;                                          \
    int runs_min = 10;                                          \
    int ms_total = 0, runs = 0;                                 \
    while (ms_total < ms_min || runs < runs_min) {              \
      long bef = gettm();                                       \
      (void)f;                                                  \
      long aft = gettm();                                       \
      ms_total += aft-bef;                                      \
      runs++;                                                   \
    }                                                           \
    printf("Elapsed time %s %d (avg of %d): %6.1f ms\n",  \
           s, N, runs, (double)ms_total/runs);                  \
  }

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
    if (strcmp(argv[optIdx],"-l") == 0) {
      printf("Enabling logging of qsim program on <stderr>\n");
      lqsim = 1;
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

// QSIM code generation
int qsdepth = 0;   // depth state

void qs_init(int q) {
  if (lqsim > 0) {
    fprintf(stderr,"%d\n",q);
  }
}

void qs_step() {
  if (lqsim > 0) {
    qsdepth++;
  }
}

void qs_gate1_ (char* g, int q) {
  if (lqsim > 0 ) {
    fprintf(stderr,"%d %s %d\n",qsdepth,g,q);
  }
}

void qs_gate1 (char* g, int q) {
  qs_gate1_(g,q);
  qsdepth++;
}

void qs_gate2_ (char* g, int p, int q) {
  if (lqsim > 0 ) {
    fprintf(stderr,"%d %s %d %d\n",qsdepth,g,p,q);
  }
}

void qs_gate2 (char* g, int p, int q) {
  qs_gate2_(g,p,q);
  qsdepth++;
}

void qs_gate1r_ (char* g, int p, double r) {
  if (lqsim > 0 ) {
    fprintf(stderr,"%d %s %d %f\n",qsdepth,g,p,r);
  }
}

void qs_gate1r (char* g, int p, double r) {
  qs_gate1r_(g,p,r);
  qsdepth++;
}

void qs_gate2r (char* g, int p, int q, double r) {
  if (lqsim > 0 ) {
    fprintf(stderr,"%d %s %d %d %f\n",qsdepth++,g,p,q,r);
  }
}

void qs_mcgate (char* g, int p, int q, int r) {
  if (lqsim > 0 ) {
    fprintf(stderr,"%d c ",qsdepth++);
    for(int i=p; i <= q; i++) {
      fprintf(stderr,"%d ",i);
    }
    fprintf(stderr,"%s %d\n",g,r);
  }
}
