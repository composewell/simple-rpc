# Upstream issues

Defects in dependencies found while testing simple-rpc, with the
workaround used here, if any.

## streamly-process: input written after the process exits

Version: streamly-process 0.4.1. Fixed in the streamly-process master
branch by commit 8653b39, "Stop writing the input when the process closes
its stdin", not released as of 2026-11-04.

`pipeChunksEither` keeps writing the input stream to the stdin of the
process after the process has exited. When the input is larger than the
pipe buffer, the write fails and the output stream raises an `IOError`:

    fd:18: hPutBuf: resource vanished (Broken pipe)

The exit code of the process is not checked, the `ProcessFailure`
exception is not raised, and the stderr of the process is lost.

In simple-rpc, `runAt` writes the arguments of the call to the server
process. A server that exits without reading its whole input, with an
argument of 1 MB, makes `runAt` raise the broken pipe error instead of
`ProcessFailure`. The test "runAt raises ProcessFailure when the server
ignores the input" is marked as an expected failure.

Workaround: `invokeFunction` reads the whole input line before it raises
the `EndpointNotFound` exception, so that a server without the endpoint
does not cause the error. A server that fails for another reason before
reading its input, e.g. a missing shared library, still causes it. The
`runAt` documentation states this limit.

Suggested fix: release the fix of the master branch.

## streamly-core: functions used here that only internal modules export

Version: streamly-core 0.3.3.

simple-rpc imports the following functions from `Streamly.Internal.*`
modules because no public module exports them. Internal modules can
change without a major version bump, so a minor release of streamly-core
can break the build.

| Function | Internal module | Used in |
|---|---|---|
| `compactEndByLn_` | `Streamly.Internal.Data.Array` | `rpc/lib/Simple/RPC/Client.hs`, to split the server output into lines |
| `ordNub` | `Streamly.Internal.Data.Stream` | `generate/app/Main.hs` |

Suggested fix: export these functions from `Streamly.Data.Array` and
`Streamly.Data.Stream`, or name the public replacements.
