# Constraint-Solver-with-Backpropagation

A report of this project can be found [here](report/.aux/main.pdf)

# Run the project
Before launching the project, run the following command to install all the needed dependencies by executing the file `./dependencies.sh`

## Command options

```
A constraint solver using AC-[3,4,6,2001] filtering algos
  -ac          Set the filtering algo among 3, 4, 6, 2001 - default : 3
  -v           Set the verbose mode
  -f           Set the input file
  -first       Stop after the first valid solution
  -queens [N]  Set the size of the queen solver
  -all-int [N] Set the size of the allIntervalSeries solver
  -only-stats  Only print the number of fails and the number of solutions
  -only-valid  Print only the valid solutions (not the fails)
  -d           Debug mode
  -print-inp   Print the input graph
  -help        Display this list of options
  --help       Display this list of options
```

command example:

run all filter algos on every input graph:
  `for graph in $(ls ./graphs/*.txt); do echo "$graph"; algo=(3 4 6 2001); for i in ${algo[@]}; do echo "$i";  ./_build/default/bin/main.exe -f $graph -ac $i -only-valid; done; done`

run ac-3 on queen 3:
  `dune exec -- main -queens 3 -ac 3 -only-valid`

run queen with n from a to b: `./queens.sh a b`

run allIntervalSeries with n from a to b = `./allInt.sh a b`

## Input file example

```
var1: val1 val2 ... valN
var2: val1 val2 ... valM
...
--
var1 val1 var2 val1
var1 val2 var2 val2
...
```

- `var1: val1 val2 ... valN`: declaration of the variable `var1` followed by the values of its domain (`val1 val2 ... valN`)
- `var1 val1 var2 val1`: `val1` of `var1` and `val2` of `var1` support each other