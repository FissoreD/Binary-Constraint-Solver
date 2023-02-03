dune build; 
msg="All Interval Series for : "

i="${1:-3}"
stop="${2:-11}"

algo=(3 4 2001 6);

echo "All Interval Series"

while [ "$i" -le "$stop" ]; do

 echo -e "\033[0;35m$msg$i\033[0m"; 

 for al in ${algo[@]}; do 

  echo "Running AC-$al"; 
  ./_build/default/bin/main.exe -all-int $i -ac $al -only-sol; 

  done; 

  i=$((i + 1))
done