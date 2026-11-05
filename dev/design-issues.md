# Design issues

Problems with the design of simple-rpc, with proposed changes that have a
wide design impact. The proposed changes are not implemented.

## ssh host key checking is disabled

Every ssh command uses `-o "StrictHostKeyChecking off"`. ssh then accepts
the key of a host it has not seen before without asking, and a man in the
middle on the first connection is not detected. Proposal: make the option
a field of `RunningConfig`, with the default `accept-new`, which accepts
only the key of a new host and refuses a changed key.

## Two processes for each call

`runAt` runs the server executable twice for each call, once without
arguments for the version check and once for the endpoint. For a remote
host this is two ssh connections for each call. The version check also
runs without `sudo` and `su`, it can check a different executable from
the one that the endpoint call runs, e.g. when the `PATH` of the user
differs.

Proposal: the client passes the expected version as an argument of the
endpoint call, e.g. `EXE --version VERSION ENDPOINT`. The server compares
`VERSION` with its own version before it reads stdin, and exits with an
error if they differ. One process, and one ssh connection, for each call.
The version is checked by the process that runs the endpoint, with the
same `sudo`, `su` and `PATH`. The server does the comparison, therefore
the output of the shell startup files of the ssh user does not affect it.

The client must distinguish a version mismatch from other server errors,
to raise `VersionMismatch` with the version of the server. The server
needs a distinct exit code or the structured error line proposed in
"Server errors reach the client only as an exit code".

A server of the current version raises `InvalidUsage` for more than one
argument, before it runs the endpoint. A new client calling an old server
fails without running the endpoint, but only with `ProcessFailure 1`. The
client can either require a new server or, after this failure, fall back
to the separate version check.

`installOnRemote` can use the same mechanism to check the remote copy,
see "installOnRemote copies the local executable": call an endpoint that
does nothing, with the version argument.

## Identify the executable by its sha256

Goal: the version of the server executable is the sha256 of the
executable file, instead of a string chosen by the user, e.g. `"0.1.0"`,
and passed to `mainWith`.

A user chosen version string does not detect a changed executable when
the user does not change the string, e.g. a rebuild after a code change.
Two builds of the same source give different sha256 values, and the
check then requires exactly the file that the client expects.

* The server computes the sha256 of its own file, from
  `getExecutablePath`, only when the client passes the expected version.
  Hashing reads the whole file on each call, an estimated 50 to 100 ms for
  a 50 MB executable, not measured.
* The server can cache the hash in a file keyed by the path, size,
  modification time and status change time (ctime) of the executable. A
  user cannot set the ctime, a replaced file gets a new ctime and is
  hashed again. The cache must be in a directory that the server user can
  write, e.g. `$XDG_CACHE_HOME`, not next to the executable, which is in a
  directory owned by root after `installOnRemote`. Add the cache only if
  the measured cost of hashing matters.
* Alternative, not preferred: embed a build ID in the executable at build
  time.
* The client gets the expected sha256 from the local executable, or from
  the user when the executable for the remote host is built elsewhere,
  e.g. for another OS or CPU architecture.
* The hash is hexadecimal, it needs no quoting on the command line.
* `installOnRemote`, or a replacement for it, can compare the sha256 of
  the remote file with the local one, and copy the executable only when
  they differ, see the `XXX` notes in `Simple.RPC.Internal.Types` and
  `Simple.RPC.Client`.

## The result is the last line of stdout

The output of the endpoint and the result share stdout, the client takes
the last line as the result. Output that reaches stdout after the result,
e.g. from a background process started by the endpoint that inherited
stdout, replaces the result. Logs of a nested `runAt` call inside an
endpoint, written with the default `putStrLn` logger, are also part of
this stdout. Proposal: write the result after a marker line that the
endpoint output cannot produce, or on a separate file descriptor for a
local server.

## Server errors reach the client only as an exit code

An `EndpointNotFound` or `InvalidInput` exception in the server makes the
client raise `ProcessFailure 1`, the message is only in the logged
stderr. Proposal: the server writes a structured error line, and the
client raises the corresponding `RpcException`.

## The input and output are buffered

The server reads the whole input line into memory, the client keeps the
whole last output line and the whole stderr of the server in memory.
`invokeFunction` reads stdin in blocks, the bytes after the input line in
the last block are lost to the endpoint. Proposal: stream the arguments
and the result, and log the stderr lines as they arrive.

## setUser and useSudo together

With both, the endpoint is run as `sudo su USER -c 'sudo EXE ...'`, the
user must be allowed to use sudo. A source comment already notes that the
two options should not be combined. The user is also given in two places,
`user@host` in `setRemote` and `setUser`. Proposal: one field with a sum
type: the current user, another user with `sudo -u USER`, or root.

## Types of the exported fields

`evaluator` returns an `RpcEval`, and the `call` field of `RpcSymbol`
takes a `Runner`. Neither type is exported, a user cannot write a type
signature for these values, and Haddock does not link them. Proposal:
export both types, `RpcEval` without its constructor.

`rcImageFlags` is a `String` split at white space, a flag cannot contain
white space. `rcRemote` is a pair of strings, the port is a `String`.
Proposal: `[String]` for the flags and a record with a numeric port for
the remote.

## installOnRemote logging and privileges

`installOnRemote` logs with `putStrLn`, a `RunningConfig` logger is not
used, and it needs sudo even for a directory that the ssh user can write.
Proposal: take a `RunningConfig`, use its logger, and use sudo only when
`useSudo` is set.

## Windows

The client and the server run on Windows, and a Windows client can call a
POSIX host with `setRemote`. The following do not work on Windows, the
documentation states the limits:

* `useSudo` and `setUser` without `setRemote` run `sudo` and `su` on the
  local host. Windows has no `su`, and the `sudo` of Windows 11 has
  different options.
* A Windows remote host: the remote commands are POSIX shell commands, the
  default shell of the Windows OpenSSH server is `cmd.exe`.
* `installOnRemote` from a Windows client, see the next section.
  `scpLocalPath` also prefixes `./` to a path with a drive letter, e.g.
  `C:\build\server.exe`, because of the colon.

Proposal: raise a named exception for the unsupported cases on Windows
instead of running a command that fails, and do not prefix `./` to a path
with a drive letter on Windows.

## installOnRemote copies the local executable

`installOnRemote` runs the local executable for the version check and
copies the same file to the remote host. The remote host must run that
file: the same OS and CPU architecture, and, for a dynamically linked
executable, compatible versions of the shared libraries, e.g. glibc,
libgmp and libffi. A macOS executable does not run on Linux, an executable
from a Linux distribution with a newer glibc may not run on an older one,
and a Windows executable does not run on a POSIX host. The local host must
also run it, for the version check.

`installOnRemote` does not run the remote copy, the failure shows up at
the first `runAt`: the version check exits with a non-zero exit code and a
loader error on stderr, and `runAt` raises a `ProcessFailure` exception.

Proposal: check the version on the remote host after the copy, so that an
executable that does not run there is reported by `installOnRemote`, and
do not run the local executable. Document that the executable must be
built for the remote host, e.g. statically linked.

## External commands

The external commands that each mode needs are listed in `design.md`.
Changes that remove some of them:

* `scp` and `mktemp` in `installOnRemote`. The executable can be sent on
  the stdin of a single ssh command, e.g.
  `t=$(mktemp) && cat > "$t" && sudo mkdir -p DIR &&
  sudo install -m 755 "$t" PATH; r=$?; rm -f "$t"; exit $r`.
  This removes `scp` and `scpLocalPath`, the `./` prefix that
  `scpLocalPath` adds to a Windows path with a drive letter, and two of the
  three ssh connections. `mktemp` is still used, but in the same shell, no
  path is returned to the client.
* `su` in `setUser`. `sudo -u USER EXE ARGS` runs the executable as the
  user without the shell of the user: no POSIX shell is needed for the
  user, no quoting, and the rc file of the user is not run. It is the
  "another user" case of the sum type proposed in "setUser and useSudo
  together".
* Shell startup output in the version check. With the expected version
  passed as an argument of the endpoint call, proposed in "Two processes
  for each call", the server compares the version, the client does not
  parse it from stdout.

Commands that stay:

* `ssh` on the local host for `setRemote`. Replacing it with an ssh library
  would add a C dependency, e.g. libssh2, and lose the user ssh config,
  agent and keys that the `ssh` command uses.
* A shell on the remote host. ssh always passes the command to the login
  shell of the ssh user. Only the quoting depends on the shell; quoting for
  `cmd.exe` would allow a Windows remote host, see "Windows".
* `sudo` for `useSudo`, `setUser`, and `installOnRemote` to a directory
  that the ssh user cannot write.
