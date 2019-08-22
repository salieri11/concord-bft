curl -k -X POST -H "Content-type:application/json" --data '{"jsonrpc": "2.0", "method": "eth_blockNumber", "params": [], "id": 1}' $1://$2:$3/
