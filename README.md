## Trial chain

A mock implementation for a toy API that supports publishing and retrieving blockchain transaction.

### Building

Use cachix to avoid building lots of dependencies (no, that's not a typo):
```
cachix use elvecent-presonal
```

To build:
```bash
nix-shell --run "cabal build all --disable-optimization"
```
To run:
```bash
nix-shell --run "cabal run trial-chain --disable-optimization"
```

To test:
```bash
nix-shell --run "cabal test --disable-optimization"
```

### Hacking

Enter the shell with

```bash
nix-shell --arg devMode true
```
