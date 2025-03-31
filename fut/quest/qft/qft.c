/** @file
 * Implements QFT algorithm
 *
 * For compilation, see ../Makefile and ../README.md
 *
 * @author Martin Elsman
 */

#include "../quest_util.h"

void init(Qureg qr, int k, int n) {
  for (int q=0; q < k; q++) {
    if (n % 2) {
      pauliX(qr,q);
    }
    n /= 2;
  }
}

void swapG(Qureg qr,int p,int q) {
  if (p==q) { return; }
  swapGate(qr,p,q);
  return;
}

void qft_swaps(Qureg qr,int k) {
  for(int q = 0; q < k / 2; q++) {
    swapG(qr,q,k-q-1);
  }
}

void qft_rots(Qureg qr, int k) {
  for (int n = k - 1; n >= 0; n--) {
    hadamard(qr,n);
    for (int q = 0; q < n; q++) {
      swapG(qr,n,q+1);
      controlledPhaseShift(qr,q,q+1,
			   M_PI/(double)pow(2,n-q));
      swapG(qr,n,q+1);
    }
  }
}

// k : Number of qubits
// n : Number to encode
double qft (int k,int n) {
  QuESTEnv env = createQuESTEnv();
  Qureg qr = createQureg(k, env);
  initZeroState(qr);
  init(qr,k,n);
  qft_rots(qr,k);
  qft_swaps(qr,k);
  int s = 0;
  double r = getProbAmp(qr, s);
  destroyQureg(qr, env);
  destroyQuESTEnv(env);
  return r;
}

int
main(int argc, char* argv[]) {
  int k = runopts(argc,argv);
  int n = 669;
  printf("Computing QFT(%d)(%d)\n", k, n);
  long tic = gettm();
  double r = qft(k,n);
  if (timing > 0) {
    reporttm("qft",k,tic);
  }
  printf("Prob of first state = %f\n", r);
  return 0;
}
