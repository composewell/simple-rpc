{ nixpkgs }:
{
# Haskell dev packages. Install only dependencies of these in the shell,
# do not build the packages themselves.
dev-packages =
  with nixpkgs.haskellPackages;
  [ simple-rpc simple-rpc-generate example ];
}
