# Design Document

## Basic Design

There are 3 parts to `simple-rpc`.

1. Defination of the RPC function
2. Registering the RPC function in the server
3. Using the RPC function


## Current Implementation

### Defination

We define the RPC version of a function by using the template haskell helper
`rpcExport`.

`rpcExport` creates the corresponding RPC function.

Example,
```
module Action.Users (ensureDirectoryForUser) where

import Streamly.Internal.Unicode.String (str)
import qualified Streamly.Internal.System.Command as Cmd
import Simple.RPC.Server

raw_ensureDirectoryForUser :: String -> FilePath -> IO ()
raw_ensureDirectoryForUser user dir = do
    Cmd.toStdout [str|sudo mkdir -p #{dir}|]
    Cmd.toStdout [str|sudo chown -R #{user} #{dir}|]
$(rpcExport "raw_ensureDirectoryForUser" "ensureDirectoryForUser")
```

### Registeration

The executable that registers all the RPC functions (aka. the server) should be
of the following form.

```
module Main
    ( main
    ) where

import qualified Action.Users as Users
import Simple.RPC.Client

main :: IO ()
main =
    mainWith "0.3.0"
        [ evaluator Users.ensureDirectoryForUser ]
```

### Usage

```
module LocalLib (func) where

import Data.Function ((&))
import qualified Action.Users as Users

rpcConfig :: RunningConfig
rpcConfig = exec exeOnRemote & onSSH sshConf
    where
    sshConf = SSHConfig "user@remote-machine.com" 22
    exeOnRemote = Executable "/opt/ssh-rpc/orchestrate" "0.3.0"

func :: IO ()
func =
    ...
    call Users.ensureDirectoryForUser rpcConfig "username" "~/.config"
    ...
```

### Problems with the current implementation

1. During the definations, we have to manually export the RPC versions.
2. During the creation of the server executable we have to make sure all the RPC
   functions are exported.

Both the above tasks can be automatic. Or atleast can be verified.

## Design Goal

Make the generation of rpc versions of exported functions more easier.

### Scratch notes on one method of solving the above problem

**User input:**

- `.cabal` file template. Like a `config.in` file. Call it `.cabal.in`
- source code `.hs` files

**User output:**

- `RPC.*` versions of all the corresponding modules
- `.cabal` file that also contains all the RPC modules and an rpc-executable

**Required components:**

- A way to get the type signatures of all the exported functions
  - Option 1: Parse the `.hi` files
  - Option 2: the `.hs` files

- A way to understand the `.cabal` file
  - Parse the `.cabal` file and create the AST
  - Modify the AST and create the final `.cabal` file
