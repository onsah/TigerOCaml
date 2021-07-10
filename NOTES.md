# Update lex file
```
cd scripts/rules
deno run --allow-read --allow-write generate.ts
```

# Update token declarations
```
cd scripts/tokens
deno run --allow-read --allow-write gen_tokens.ts
```

## Format all project files
```
dune build @fmt --auto-promote
```