# Design

The current design of simple-rpc, as implemented. Problems with the design and
proposed changes are in `design-issues.md`.

## Call mechanism

An endpoint call with `runAt` runs the server executable as a process, on
the local host or on a remote host with ssh:

1. Version check: the client runs the executable without arguments and
   compares its stdout with `executableVersion`.
2. Endpoint call: the client runs `executable <image flags> <endpoint>`,
   writes the JSON array of the arguments to its stdin, and decodes the
   last line of its stdout as the result. The stderr of the server is
   buffered and logged after the server exits.

The server, `mainWith`, reads the endpoint name from its arguments and the
input from stdin, and writes the output to stdout. It does not run any
external command.

The client starts processes with `streamly-process`, which executes the
program directly, without a shell. A shell parses the command line only on
the remote host, where ssh passes the command to the login shell of the
ssh user, and under `su -c`, which passes the command to the shell of the
user. The client quotes these command lines for a POSIX shell.

## External commands

All external commands are run by `Simple.RPC.Client`. The server and the
other modules do not run any.

| Mode | Local host | Remote host |
|---|---|---|
| Local call | the executable | |
| Local call, `useSudo` | `sudo` | |
| Local call, `setUser` | `sudo`, `su`, POSIX shell for the user | |
| `setRemote` | `ssh` | POSIX login shell for the ssh user |
| `setRemote`, `useSudo` | `ssh` | POSIX login shell, `sudo` |
| `setRemote`, `setUser` | `ssh` | POSIX login shell, `sudo`, `su`, POSIX shell for the user |
| `installOnRemote` | `ssh`, `scp`, the executable | POSIX login shell, `mktemp`, `rm`, `sudo`, `mkdir`, `install` |

The commands built for each mode:

* `useSudo`: `sudo EXE ARGS`.
* `setUser USER`: `sudo su USER -c 'EXE ARGS'`. With `useSudo` as well:
  `sudo su USER -c 'sudo EXE ARGS'`.
* `setRemote (ADDR, PORT)`: `ssh -T -p PORT -o "StrictHostKeyChecking off"
  ADDR 'CMD'`, where `CMD` is one of the commands above, quoted for a POSIX
  shell.
* The version check is run without `sudo` and `su`, with ssh if
  `setRemote` is used.
* `installOnRemote` runs `mktemp` with ssh, copies the executable to the
  temporary file with `scp`, and then runs, with ssh,
  `sudo mkdir -p DIR && sudo install -m 755 TMP PATH; r=$?; rm -f TMP;
  exit $r`. If `scp` fails, it removes the temporary file with
  `ssh ... rm -f TMP`. The version check runs the local executable.

Output of the shell startup files of the ssh user on stdout is part of the
version check output, and the version check fails.
