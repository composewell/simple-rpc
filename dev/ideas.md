# Ideas

## Naming

* Alternative names for `runAt`: `call`, `callAt`, or `cmd` and `cmdAt`.
  `call` is currently the name of the internal function that `runAt` is
  built on (`runAt cfg sym = call sym (with cfg)`), so that function
  would need to be renamed too.
* Rename the package to `command-rpc` or `shell-rpc`.

## Install the server without sudo

Instead of installing the server executable to a designated location
with `sudo`, copy it to a directory owned by the ssh user and run it
from there. Then the remote machine needs no passwordless `sudo` for
installation.

Possible locations:

* `/tmp`: often mounted `noexec`, emptied on reboot, and shared by all
  users. Another user can create or replace a file at a fixed path such
  as `/tmp/example-server`, and the caller then runs that user's code. A
  directory created with `mktemp -d` avoids this, but its name has to be
  remembered between calls.
* A directory in the home of the ssh user, e.g. `~/.cache/simple-rpc`.

## Install on demand

If `runAt` does not find the server executable on the remote machine,
or finds a different version, it copies the executable and then makes
the call. This needs the sudo-free location above.

* Currently a missing executable shows up as a failed version check, a
  `ProcessFailure` exception with exit code 127 from ssh. The client has
  to tell this apart from other failures.
* Compare a sha256 of the executable instead of a version string, so
  that any change to the executable is detected.

See also the `XXX` comment above `defaultConfig` in
`rpc/lib/Simple/RPC/Internal/Types.hs`.

## Client and server in one executable

The client and the server are the same program, compiled once for each
OS and architecture of the machines it runs on. Each build is named
with a `.os.arch` suffix, e.g. `deploy.linux.x86_64`,
`deploy.linux.aarch64` and `deploy.darwin.aarch64`.

To call a remote machine, the client finds its OS and architecture, e.g.
with `uname -s -m` over ssh, and copies the build with the matching
suffix if it is not already there. When the remote machine has the same
OS and architecture as the client, the client copies itself, using
`getExecutablePath` to find its own file.

The builds for one OS and architecture must also be compatible with the
shared libraries of the remote machines, e.g. glibc, unless they are
statically linked.

## Built-in ssh client

The client could use an ssh client library linked into the executable
instead of the external `ssh` command.

* For: the client machine does not need an `ssh` executable.
* For: the client does not start an `ssh` process for each call.
* For: the client can set defaults needed by simple-rpc, e.g. agent
  forwarding for remote to remote calls. Agent forwarding should still
  be enabled only when the user explicitly asks for it.
* Against: ssh is ubiquitous, so this may not be worth doing.
