
find src -name "*.rs" | while read f; do
  echo "file = \"$f\""
  echo "\`\`\`rust"
  cat $f
  echo "\`\`\`"
done > src.txt

ls -1 Cargo.toml | while read f; do
  echo "file = \"Cargo.toml\""
  echo "\`\`\`toml"
  cat $f
  echo "\`\`\`"
done >> src.txt
