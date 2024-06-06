# CAUTION! a spelling mistake in arg string is ignored silently.
#
# nix-build --arg static true
# vector takes too much memory in compiling tests. We need to disable tests to
# build it.
# nix-build --arg static true --arg nixpkgs 'import ~/third-party/nixpkgs {}'

{
  nixpkgs ?
    import
      (builtins.fetchTarball
        https://github.com/composewell/nixpkgs/archive/refs/tags/22.05-composewell.tar.gz
      )
      {}
, compiler ? "ghc922"
, c2nix ? "" # cabal2nix CLI options
, static ? false
}:
let
    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # When in shell, avoid copying the source directory
                    # to nix store by using src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    flags = "--benchmark --flag fusion-plugin" + " " + c2nix;

    mkHaskellPackages = pkgs: hpkgs: inShell:
        hpkgs.override {
            overrides = self: super:
                with pkgs.haskell.lib;
                {
                    simple-rpc-setup =
                      pkgs.haskell.lib.overrideCabal
                        (mkPackage super "simple-rpc-setup" ./setup/. flags inShell)
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    simple-rpc-generate =
                      pkgs.haskell.lib.overrideCabal
                        (mkPackage super "simple-rpc-generate" ./generate/. flags inShell)
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    simple-rpc =
                      pkgs.haskell.lib.overrideCabal
                        (mkPackage super "simple-rpc" ./rpc/. flags inShell)
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    example =
                      pkgs.haskell.lib.overrideCabal
                        (mkPackage super "example" ./example/. flags inShell)
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    streamly =
                      pkgs.haskell.lib.overrideCabal
                        (super.callHackageDirect
                          { pkg = "streamly";
                            ver = "0.9.0";
                            sha256 = "sha256-eOxVb8qQjZDo1+S7CStqYSExOg2QHWkMY+zlOYqwZak=";
                          } {})
                        #(let src = fetchGit {
                        #    url = "git@github.com:composewell/streamly.git";
                        #    rev = "d33bb110ab5ed6a8cbd8f49e23517df2dbcf3c3b";
                        #}; in super.callCabal2nix "streamly" src {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [pkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    streamly-core =
                      pkgs.haskell.lib.overrideCabal
                        (super.callHackageDirect
                          { pkg = "streamly-core";
                            ver = "0.1.0";
                            sha256 = "sha256-hoSV6Q2+X5a7hFnJAArqNPjcMaCVyX9Vz4FcxeJ+jgI=";
                          } {})
                      #pkgs.haskell.lib.overrideCabal
                      #  (let src = fetchGit {
                      #      url = "git@github.com:composewell/streamly.git";
                      #      rev = "d33bb110ab5ed6a8cbd8f49e23517df2dbcf3c3b";
                      #  }; in super.callCabal2nix "streamly-core" "${src}/core" {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [pkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    streamly-coreutils =
                      pkgs.haskell.lib.overrideCabal
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly-coreutils.git";
                            rev = "d36e0810b9f091eafaadc183a02de9c0cce6eada";
                        }; in super.callCabal2nix "streamly-coreutils" src {})
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                            doCheck = false;
                          });

                    streamly-process =
                      pkgs.haskell.lib.overrideCabal
                        (super.callHackageDirect
                          { pkg = "streamly-process";
                            ver = "0.3.0";
                            sha256 = "sha256-h0BN+L0nFQqhgnlOk8bbivteqxS7ljhckHA51RLyibk=";
                          } {})
                      #pkgs.haskell.lib.overrideCabal
                      #  (let src = fetchGit {
                      #      url = "git@github.com:composewell/streamly-process.git";
                      #      rev = "7ef84bddc869c6a7c46fd5e4ef9c1b4a22c301db";
                      #  }; in super.callCabal2nix "streamly-process" src {})
                        (old:
                          { enableLibraryProfiling = false;
                            doHaddock = false;
                            doCheck = false;
                          });

                    lockfree-queue =
                      super.callHackageDirect
                        { pkg = "lockfree-queue";
                          ver = "0.2.4";
                          sha256 = "1bj9agy3x0yjbscpjgn96gpnj4lvkh39spjvy3jnrr3a42v3ynw7";
                        } {};

                };
        };

    shell = pkgs: hpkgs:
      hpkgs.shellFor {
        packages = p:
          [ p.simple-rpc
            p.simple-rpc-setup
            p.simple-rpc-generate
            p.example
          ];
        doBenchmark = true;
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };

   getHaskellPackages = pkgs:
      let h =
         if compiler == "default"
         then pkgs.haskellPackages
         else pkgs.haskell.packages.${compiler};
      in mkHaskellPackages pkgs h false;

   drvBuild =
     if static
     then
      let pkgs = nixpkgs.pkgsMusl;
          hpkgs = getHaskellPackages pkgs;
       in pkgs.haskell.lib.overrideCabal hpkgs.simple-rpc
           (old:
             { configureFlags =
                 [
                   "--ghc-option=-optl=-static"
                   "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                   "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                   "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                   "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
                 ];
               enableSharedExecutables = false;
               enableSharedLibraries = false;
               enableLibraryProfiling = false;
               doHaddock = false;
             })
       else (getHaskellPackages nixpkgs).simple-rpc;

   drvShell =
     if static
     then
      let pkgs = nixpkgs.pkgsMusl;
          hpkgs = getHaskellPackages pkgs;
       in shell pkgs (getHaskellPackages pkgs)
     else shell nixpkgs (getHaskellPackages nixpkgs);
in if nixpkgs.lib.inNixShell
   then drvShell
   else drvBuild
