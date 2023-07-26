git submodule update --recursive --remote

rm -rf lura-syntax/src/generated

type-sitter-cli vendor/tree-sitter-lura/src/node-types.json \
  -t node-types \
  --output-dir lura-syntax/src/generated \
  --language tree-sitter-lura
