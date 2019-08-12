curl -k -X POST -H "Content-type:application/json" --data '{"jsonrpc":"2.0","method":"eth_getBlockByNumber","params":["'""$1""'", true],"id":1}' https://$2:8545/
