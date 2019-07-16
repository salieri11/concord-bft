const Web3 = require('web3')

module.exports = {
  networks: {
    development: {
      network_id: "*",
      provider: () => {
          const HttpHeaderProvider = require('httpheaderprovider');
          const Web3 = require('web3');
          const web3 = new Web3();

          const toBase64 = function (data) {
            const buff = new Buffer(data);
            return buff.toString('base64');
          }

          const basicAuthEncode = function (user, pass) {
            const header = user + ':' + pass;
            return 'Basic ' + toBase64(header)
          }

          const basicAuth = basicAuthEncode("USER_PLACEHOLDER", "KEY_PLACEHOLDER");
          const header = {'authorization': basicAuth};
          const provider = new HttpHeaderProvider('http://localhost:8080/api/concord/eth/', header);
          return provider;
      }
    }
  },
  compilers: {
    solc: {
      version: "0.5.2",
    }
  }
}

