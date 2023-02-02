# Constraint-Solver-with-Backpropagation

command example: 

run ac3 over all input graphs:
  `for f in ./graphs/*; do echo $f; ./_build/default/bin/main.exe -f $f -ac 3 | tail -n 3; done`

run all filter algos on input_4:
  `algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  ./_build/default/bin/main.exe -f graphs/input_4.txt -ac $i | tail -n 3; done`

run all filter algos on every input graph:
  `for graph in $(ls ./graphs/*.txt); do echo "$graph"; algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  ./_build/default/bin/main.exe -f $graph -ac $i -only-sol; done; done`

run ac-3 on queen 3:
  `dune exec -- main -queens 3 -ac 3 -only-sol`

run queen algo from 3 to 11 for all algos: 
  `dune build; for i in {3..11}; do echo "Queen number : $i"; algo=(3 4 6 2001); for al in ${algo[@]}; do echo "Running AC-$al"; ./_build/default/bin/main.exe -queens $i -ac $al -only-sol; done; done`

run allIntervalSeries with n from 3 to 11:
  `dune build; for i in {3..11}; do echo "Interval Series with n : $i"; algo=(3 4 2001 6); for al in ${algo[@]}; do echo "Running AC-$al"; ./_build/default/bin/main.exe -all-int $i -ac $al -only-sol; done; done`