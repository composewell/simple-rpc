module Module0 (exeVersion, remoteExe, localhost) where

import Simple.RPC.Client

exeVersion :: String
exeVersion = "0.1.1"

remoteExe :: Executable
remoteExe = Executable "/opt/ssh-rpc/test" exeVersion

localhost :: SSHConfig
localhost = ("user@localhost", "22")
