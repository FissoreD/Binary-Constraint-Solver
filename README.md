# Constraint-Solver-with-Backpropagation

command example: 

run ac3 over all input graphs:
  `for f in ./graphs/*; do echo $f; dune exec -- main -f $f -ac 3 | tail -n 3; done`

run all filter algos on input_4:
  `algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  dune exec -- main -f graphs/input_4.txt -ac $i | tail -n 3; done`

run all filter algos on every input graph:
  `for graphs in $(ls ./graphs/*.txt); do algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  dune exec -- main -f $graphs -ac $i | tail -n 3 ; done; done`

run queen algo from 3 to 8 for all algos: 
  `for i in {3..8}; do echo "Queen number : $i"; algo=(3 4 6 2001); for al in ${algo[@]}; do echo "Running AC-$al"; dune exec -- main -queens $i -ac $al -count-only; done; done`