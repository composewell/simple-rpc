# Issues

Implementation bugs and cleanups that are not design issues.

## simple-rpc-generate writes invalid Haskell when no function is found

When the source directory has no RPC function, the generated module has
`rpcMap = createRpcMap` followed by a line with only `]`, which does not
compile. Reproduced with a config whose source directory is empty.

## simple-rpc-evaluate does not compile

`evaluate/app/Main.hs` imports `Simple.RPC.Types` and uses `SSHConfig`
and `with`, which no longer exist. The package is not in
`cabal.project`. Observed in the source, not built.

## Remote and privileged calls are tested only with fake commands

The tests run `ssh`, `scp`, `sudo` and `su` as fake commands, copies of
the test executable built from `rpc/test/Fake.hs`. The calls with
`setRemote`, `useSudo` and `setUser` have not been tested against the
real commands.

## The tests have not run on Windows

The Windows CI job runs the tests, with the `installOnRemote` tests
skipped, but the job has not run yet. The open question is the quoting of
the remote command through the `sh` of Git for Windows (msys).
