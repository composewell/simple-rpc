# Issues with build time processing

It is possible to make the workflow easier by processing the `.hs` files and
inducing the desired behaviour but the CPP macros are hard to handle.

To handle the CPP macros we need to process the files after the CPP phase is
done. So we need to induce a hook after the CPP processing.

The CPP processing is taken care by the GHC and hence the build tool cannot
handle this. Or atleast cabal cannot.

To this hook needs to be incuced somewhere in the GHC pipeline itself. This is
too much of a hassle. and might not work as expected.

We can possibly stick to the template haskell helpers. The only problem I have
is sometimes I add an rpc function but I forget to export it. We can maybe have
some linter to solve that problem?

## Ignoring the CPP issue

The current implementation pre-preocess the `.hs` files.

There are many pitfalls given this implementation:
1. The changes are not transparent to the user.
2. The modifications are not robust. It fails in many cases.
   - Fails with CPP
   - Fails with recursive functions
