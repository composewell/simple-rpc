# README

Look at `app` for an example application.

## Pre-requisites

* Remote machine should be accessible via ssh for some user(s). For
  example, by adding the user's public key to the user's
  $HOME/.ssh/authorized_keys file.
* To avoid putting the user's private key on each machine, ssh agent
  forwarding can be used to forward the keys from one machine to another.
* Need a `sudo` capable user on each remote machine for:
  * installing the rpc executable
  * running commands as an arbitrary user

## Usage as a CLI

```
echo '[22, "Test", true]' | cabal run example -- "Module1.printFromModule1"
```
