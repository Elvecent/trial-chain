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

### Manual testing

Start with an initial transaction that gives 100 coins to anyone:
```bash
nix-shell --run "cabal run trial-chain --disable-optimization -- 00000000000000000000000000000001010005f5e100010000000000000000"
```

Try broadcasting test data:
```bash
curl -H "Content-Type: application/json" -d @data.json localhost:8080/broadcast
```

Then read it back:
```bash
curl localhost:8080/get/434d9f5da56a86ceb0ed9901e0c03b8fd48c1497117840f44692e78b81ad8cc2
```

### Hacking

Enter the shell with

```bash
nix-shell --arg devMode true
```
