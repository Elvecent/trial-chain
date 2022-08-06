## Trial chain

A mock implementation for a toy API that supports publishing and retrieving blockchain transaction.

### Building

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
