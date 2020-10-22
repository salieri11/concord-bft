# VMware Blockchain Contract Compiler Microservice

The contract compiler microservice is a Node and Express server built to
compile and verify Solidity Smart Contracts for multiple versions of the 
solc compiler.

## Build

Build with Docker in the `contract-compiler` directory:

```bash
docker build . -t contract-compiler:latest
```

## Development

First, navigate to the `contract-compiler` directory

If you are using nvm, switch to the node version specified in the `.nvmrc` file.

```bash
nvm use
```

Next, install dependencies

```bash
npm install
```

### Startup a dev server

Standard dev server
```bash
npm start
```

### Code formatting and linting

To check the code against the rules outlined in `.eslintrc`:
```bash
npm run lint
```

To run the linter and resolve any automatically fixable issues:
```bash
npm run lint-fix
```

### Running Tests with Hermes

1. Navigate to the `blockchain/hermes` directory
1. Either start a dev server using the command above or start the `contract-compiler` image with `docker-compose up`
1. Then `python3 -m pytest suites/contract_compiler_tests.py --noLaunch`
