/** @file
 * Implements GHZ algorithm
 *
 * Compile and run from within the build folder, using:
 *  $ cmake .. -DUSER_SOURCE=../examples/ghz.c -DOUTPUT_EXE=ghz
 *  $ make
 *  $ ./ghc 5
 *
 * @author Martin Elsman
 */

#include "../quest_util.h"

double
ghz (int n,int s) {
  QuESTEnv env = createQuESTEnv();
  Qureg qr = createQureg(n, env);
  initZeroState(qr);
  hadamard (qr, 0);
  for (int i=0; i < n-1; i++) {
    controlledNot(qr, i, i+1);
  }
  double r = getProbAmp(qr, s);
  destroyQureg(qr, env);
  destroyQuESTEnv(env);
  return r;
}

int
main(int argc, char* argv[]) {
  int N = runopts(argc,argv);
  int s = pow(2,N)-1;
  printf("Computing GHZ(%d)\n", N);
  long tic = gettm();
  double r = ghz(N,s);
  if (timing > 0) {
    reporttm("ghz",N,tic);
  }
  printf("Prob of last state (state %d) = %f\n", s, r);
  return 0;
}
