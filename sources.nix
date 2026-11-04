{nixpack}:
# Package sources to override the streamly-packages set.
with nixpack.mkSources;
{
layers = [
{
  streamly-core = hackage "0.3.3"
    "0rrgbk1jmhyzgg39wd2wdli6xqdcb36xmqkqmvix7qy6rjfykicz";
  # Hackage revision 3 allows streamly-core < 0.3.4
  streamly = hackage "0.11.1"
    "1dxyhq8m9fr3ghpmxkh6bc37fd4f1048xckjrlpp6ybvlg0lq7g2"
    // { rev = "3";
         cabalSha256 = "0xvx6vacr9vlss67qs5v25gbj6xvxjzx4f7w5ck9lr845g57k9ay";
       };
  simple-rpc = local ./rpc;
  simple-rpc-generate = local ./generate;
  example = local ./example;
}
];
}
