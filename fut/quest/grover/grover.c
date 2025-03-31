/** @file
 * Implements Grover's algorithm
 *
 * For compilation, see ../Makefile and ../README.md
 *
 * @author Martin Elsman
 */

#include "../quest_util.h"

void diffuse(Qureg qr, int n, int* cntrls) {
  for (int i=0; i<n; i++) {
    hadamard(qr,i);                                              qs_gate1_("h",i);
    pauliX(qr,i);                                                qs_gate1_("x",i);
  }                                                              qs_step();
  hadamard(qr,n-1);                                              qs_gate1("h",n-1);
  multiControlledMultiQubitNot(qr,cntrls,n-1,(int[]){n-1},1);    qs_mcgate("x",0,n-2,n-1);
  hadamard(qr,n-1);                                              qs_gate1("h",n-1);
  for (int i=0; i<n; i++) {
    pauliX(qr,i);                                                qs_gate1_("x",i);
    hadamard(qr,i);                                              qs_gate1_("h",i);
  }                                                              qs_step();
}

void encNum(Qureg qr, int n, int i) {
  //for (int j=n-1; j >= 0; j--) {   // toggle lsb/msb encoding
  for (int j=0; j < n; j++) {
    if (i % 2 == 0) {
      pauliX(qr,j);          qs_gate1_("x",j);
    }
    i /= 2;
  }                          qs_step();
}

void oracle(Qureg qr, int n, int i, int* cntrls) {
  encNum(qr, n, i);
  multiControlledPhaseFlip(qr,cntrls,n);    qs_mcgate("z",0,n-2,n-1);
  encNum(qr, n, i);
}

double grover (int n,int s) {
  int cntrls[n];
  for (int i=0; i<n; i++) {
    cntrls[i] = i;
  }
  QuESTEnv env = createQuESTEnv();
  Qureg qr = createQureg(n, env);
  initZeroState(qr);                                       qs_init(n);
  int k = (int)ceil(sqrt((double)(pow(2,n))) * M_PI/4.0);
  for (int i=0; i<n; i++) {
    hadamard(qr,i);                                        qs_gate1_("h",i);
  }                                                        qs_step();
  for (int i=0; i<k; i++) {
    oracle(qr,n,s,cntrls);
    diffuse(qr,n,cntrls);
  }

  double maxprob = 0.0;
  int laststate = pow(2,n)-1;
  int maxstate = 0;
  for (int i = 0; i <= laststate; i++) {
    double r = getProbAmp(qr, i);
    if (r > maxprob) {
      maxprob = r;
      maxstate = i;
    }
  }
  printf("Found max prob at index %d : %f\n", maxstate, maxprob);
  destroyQureg(qr, env);
  destroyQuESTEnv(env);
  return maxprob;
}

int
main(int argc, char* argv[]) {
  int N = runopts(argc,argv);
  int s = 12;
  printf("Computing Grover(%d) - search for %d\n", N, s);
  long tic = gettm();
  double r = grover(N,s);
  if (timing > 0) {
    reporttm("grover",N,tic);
  }
  printf("Prob of found state = " REAL_STRING_FORMAT "\n", r);
  return 0;
}
