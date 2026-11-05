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
  # Hackage revision 3 allows streamly-core < 0.3.4
  streamly-bytestring = hackage "0.2.3"
    "05nnpcq7jqmpfzwv7giqi50i6mnzmfywxgy40d506vx2xr27n5b4"
    // { rev = "3";
         cabalSha256 = "0ciq3zj3jqj482xhgz2rwyin9hiwci8cm852s8yv254yv6dzhkdn";
       };
  simple-rpc = local ./rpc;
  simple-rpc-generate = local ./generate;
  example = local ./example;
}
];
}
