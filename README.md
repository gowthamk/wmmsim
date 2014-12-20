WMMSim : Weak Memory Model simulator
===================================

WMMSim is a solver-aided DSL to simulate weak memory models. A weak
memory model can be defined in terms of acceptable violations of a)
program order, and b) atomicity of writes (stores). WMMSim relies on
this observation to describe a memory model constructively as a
non-deterministic function that takes a shared memory concurrent
program, and non-deterministically re-orders instructions in the
program. WMMSim then symbolically interprets the resultant program to
determine if any unexpected behaviours are possible under the memory
model. If such behaviour is possible, WMMSim outputs an execution of
the shared memory program which reproduces unexpected behaviour.

Installation Instructions
=========================

WMMSim is implemented as a Rosette program, which inturn is
implemented in Racket.

1. Download Racket version 6.1 or higher from
   http://racket-lang.org/download/
2. Download and install Rosette from https://github.com/emina/rosette
3. Open wmmsim.rkt in racket ide, and click "Run". The file includes
   source code and also examples. Clicking "Run" runs WMMSim over
   examples in the file.
