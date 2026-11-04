# Revision history for simple-rpc

## Unreleased

### Breaking changes

No exported type signature has changed. The following behavior changes
can break existing callers:

* Errors are raised as an `RpcException` instead of an `ErrorCall`: an
  unknown endpoint (`EndpointNotFound`), an invalid server input
  (`InvalidInput`), an invalid client output line (`InvalidOutput`), a
  version mismatch (`VersionMismatch`) and more than one argument to the
  server (`InvalidUsage`). `EmptyOutput` is unchanged.
* The server rejects an input array with more or fewer elements than the
  arguments of the endpoint. Before, extra elements were ignored.
* The server decodes all arguments before it calls the endpoint. Before,
  an argument was decoded when the endpoint used it, an invalid argument
  that the endpoint did not use was accepted.
* `runAt` decodes the result before it returns, a decoding error is raised
  by `runAt`. Before, it was raised when the caller used the result.
* `setImageFlags` flags are passed to the server executable. Before, they
  were ignored.
* `installOnRemote` installs the executable with `sudo install -m 755`, the
  file is owned by root. It no longer changes the permissions of the
  directory. Before, the directory was made writable by all users and the
  file was owned by the ssh user. It needs `mktemp` and `install` on the
  remote host.
* The server sets the encoding of stdout to UTF-8, and writes `\n` line
  ends on all OSes. Before, it wrote `\r\n` on Windows.

### Additions

* `RpcException` is exported from `Simple.RPC.Common`.

### Bug fixes

* A non-ASCII character in a `String` argument or result made the call
  fail.
* The server failed in a non-UTF-8 locale, e.g. `LC_ALL=C`, when the
  input, the endpoint output or the version had a non-ASCII character.
* An executable path, a user name or an argument with a space, a quote,
  `$`, a backtick, a backslash or a non-ASCII character was not quoted
  correctly for the local command, `su -c` and ssh.
* `installOnRemote` left the remote directory writable by all users, and
  failed when the executable was running or the path had a space.
* `rpcExport` of a function named `val` did not compile.
* For an unknown endpoint and an input larger than the pipe buffer, `runAt`
  raised a broken pipe error instead of `ProcessFailure`.
* `toBinStream` took time proportional to the size times the number of
  chunks of the encoding, 24 MB of JSON took 20 seconds. The client and
  the server serialization use the chunks of the encoding directly.

### Documentation

* Corrected the defaults of `setRemote` and `setUser` and documented the
  protocol, the exceptions and the limits of `runAt`, `invokeFunction`,
  `mainWith`, `rpcExport` and `installOnRemote`.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
