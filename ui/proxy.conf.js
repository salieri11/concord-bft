const PROXY_CONFIG = {
  "/api": {
    "target": "http://0.0.0.0:8080",
    "headers": {
      'X-Forwarded-Proto': 'https',
      'X-Forwarded-For': 'localhost.vmware.com'
    },
    "secure": false,
  },
  "/daml-json-api": {
    "target": "http://localhost:7575",
    "headers": {
      'X-Forwarded-Proto': 'https',
      'X-Forwarded-For': 'localhost.vmware.com'
    },
    "secure": false,
    "pathRewrite": { "^/daml-json-api": "" }
  },
  "/csp": {
    "target": "https://console-stg.cloud.vmware.com",
    "secure": true
  },
  "/dapp": {
    "target": "http://localhost:4000",
    "secure": false,
    "pathRewrite": { "^/dapp": "" }
  },
  "/swagger": {
    "target": "http://localhost:8080",
    "secure": false
  },
  "/geo": {
    "target": "https://api.opencagedata.com/geocode/v1/json",
    "changeOrigin": true,
    "secure": true,
    "logLevel": "debug"
  }
}

module.exports = PROXY_CONFIG;
