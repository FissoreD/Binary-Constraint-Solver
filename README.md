# Constraint-Solver-with-Backpropagation

command example: 
`for f in ./graphs/*; do echo $f; dune exec -- main -f $f -ac 3 | tail -n 3; done`

`algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  dune exec -- main -f graphs/input_4.txt -ac $i | tail -n 3; done`