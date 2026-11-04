# README

The goal of the `generate` executable is to generate the server automatically.

**Step 1:**

Add the following ghc-options to your library sections
- `-ddump-to-file`
- `-dth-dec-file`
- Eg: `ghc-options: -ddump-to-file -dth-dec-file`

**Step 2:**

Create a rpc configuration file:

Example:
```
{
    "distBuildDir": "../dist-newstyle/build/x86_64-linux/ghc-9.10.3/example-0.1.0.0/build/",
    "sourceDirectories": ["lib"],
    "rpcModulePath": "app/RpcMap.hs",
    "rpcModuleName": "RpcMap",
    "compilationMode": "Normal"
}
```

`compilationMode` is one of `Normal`, `Dynamic` or `Threaded`; it selects
the `.th.hs`, `.dyn.th.hs` or `.thr.th.hs` dump files respectively.

**Step 3:**

Run the generator:

- Run the `cabal build <library-name>`
- Run the `<generator-exe> <rpc-config-file>`

This generates the module `rpcModuleName` at `rpcModulePath`, exporting
`rpcMap :: RpcMap IntermediateRep`. Add it to `other-modules` of the server
executable and use it in `main`:

```haskell
main :: IO ()
main = mainWith "0.1.1" rpcMap
```
