# README

The goal of the `generate` executable is to generate the server automatically.

**Step 1:**

Add the following ghc-options to your library sections
- `-ddump-to-file`
- `-dth-dec-file`
- Eg: `ghc-options: -ddump-to-file -dth-dec-file`

**Step 2:**

Add the following comments in your cabal file where you want the executable code
to be generated.

```
-- <rpc-server>
-- </rpc-server>
```

The code generated inside this is handled by the `generate` executable.

**Step 3:**

Create a rpc configuration file:

Example:
```
{
    "distBuildDir": "../dist-newstyle/build/x86_64-linux/ghc-9.2.2/example-0.1.0.0/build/",
    "sourceDirectories": ["lib"],
    "cabalFile": "example.cabal",
    "libraryName": "example",
    "serverDirectory": "app",
    "serverExecutableName": "example-server",
    "serverConfigImportList": ["lib-options"],
    "serverVersion": "0.0.0"
}
```

**Step 4**

Run the generator:

- Run the `cabal build <library-name>`
- Run the `<generator-exe> <rpc-config-file>`

This should populate the `<rpc-server>` block and generate the executable
itself.
