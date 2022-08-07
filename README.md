## Trial chain

A mock implementation for a toy API that supports publishing and retrieving blockchain transaction.

### Building

Use cachix to avoid building lots of dependencies:
```
cachix use elvecent-presonal
```

To build:
```bash
nix-shell --run "cabal build"
```
To run:
```bash
nix-shell --run "cabal run"
```

### Hacking

Enter the shell with

```bash
nix-shell --arg devMode true
```
