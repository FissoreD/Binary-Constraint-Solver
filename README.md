# Constraint-Solver-with-Backpropagation

command example: 

run ac3 over all input graphs:
  `for f in ./graphs/*; do echo $f; dune exec -- main -f $f -ac 3 | tail -n 3; done`

run all filter algos on input_4:
  `algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  dune exec -- main -f graphs/input_4.txt -ac $i | tail -n 3; done`

run all filter algos on every input graph:
  `for graps in $(ls ./graphs); do algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  dune exec -- main -f ./graphs/$graps -ac $i | tail -n 3 ; done; done`