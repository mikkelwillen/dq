# Measurements in DQ
This project is about implementing measurements in the DQ framework, and showing
some usecases for measurements in quantum computing.

By: Mikkel Willén

## Contributions
Below are the contributions this project adds to the DQ framework, shortly 
explaing what it is about, and in what files they have been implemented.

### Measurements
Measurements have been implemented in `semantics.sml` and is composed of the
functions:  

`measureQubit : state -> int -> bool * state`
which takes a state and a qubit index as input, and returns a measurement of the
qubit and the state collapsed around the result.
The measured qubit is determined, by generating a random number between `0` and 
`1`, and returning `1` if the generated number is less than the probability of
the qubit being in state `1`, or `0` otherwise.  

`measureNQubits : state -> int -> int -> bool * state`
works the same way as `measureQubit`, but generates `n` random numbers, compares
them to the probability of the qubit being in state `1`, and then returns the 
state that is most frequently measured. Then it collapses the state around that
measurement.  

Similarly `measureNQubitsDist : state -> int -> int -> bool list` 
generates a list of `n` measurement outcomes without collapsing the state.  

2 small tests of measurements have been added in the files `measure_ex2.sml` and `entangle_ex1.sml`, showing that measuring the same qubit again after collapse,
returns the same measured value, and that measuring a control qubit, will result
in the controlled qubits state to be known.

### QFT
The Quantum Fourier Transform (QFT) has been implemented in `qft.sml` and 
exposes the function `qft_circuit : int -> int -> Citcuit.t`, which given the 
inputs `k`, number of qubits and `n`, the number to be incoded, returns the 
QFT circuit encoding `n` in on a circuit with `k` qubits. 
It is based on: https://medium.com/@marcell.ujlaki/exploring-quantum-computing-demystifying-quantum-fourier-transformations-unveiling-the-math-with-5d74f3f8025f


### Repeat operator
A repeat operator has been added to the circuit type. It takes a circuit `Circuit.t` and an integer `n`, semantically means applying the circuit `n` times on the state.
