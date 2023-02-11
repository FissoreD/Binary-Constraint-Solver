dune build; 
msg="Queens number : "

i="${1:-1}"
stop="${2:-12}"

algo=(3 4 6 2001);

echo "Queens"

while [ "$i" -le "$stop" ]; do

 echo -e "\033[0;35m$msg$i\033[0m"; 

 for al in ${algo[@]}; do 

  echo "Running AC-$al"; 
  
  ./_build/default/bin/main.exe -queens $i -ac $al -only-sol; 

  done; 

  i=$((i + 1))
done