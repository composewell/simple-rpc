Any random function cannot be RPC-fied.
Only non-polymorphic functions whose input params can be serialized can be RPC-fied.

We should make it easier to create an rpc server given the user specifies the
functions that need to be RPC-fied.
This includes:
1. Exporting the required RPC-fied functions automatically
2. Importing the required RPC-fied functions automatically and create a server
