# VMware Blockchain UI

## Build

Build using docker in the ui directory:

```bash
docker build . -t ui:latest
```

## Development

To start off, go to the `ui` dir and install all of your dependencies

```bash
npm install
```

### Startup a dev server

Standard dev server
```bash
npm run start
```

Dev server with CSP flavor
```bash
npm run start:csp
```
### Running Tests
Two ways of running tests.

**Hermes**
1. Install chrome and xvfb `sudo apt-get install xvfb chromium-browser`
1. Make sure everything is running with `docker-compose up -d`
1. Export your VMC api key, found here (found here)[https://console.cloud.vmware.com/csp/gateway/portal/#/user/tokens] `export LINT_API_KEY=XXXXXXXXX`
1. Add your fluentd api key to the `docker/fluentd/fluentd.conf` file and change the below line.

```shell
Authorization Bearer <ADD-LOGINTELLIGENCE-KEY-HERE>
```

1. Instantiate a new instance `python3 reinitializeDatabases.py`
1. Then `python3 main.py UiTests --noLaunch`
1. If tests are failing you may need to relaunch with docker-compose

**Manually**
1. Unit Tests `npm run test`
1. E2E Tests `npm run e2e`
1. Linter `npm run lint`

### File Structure and Coding Style
Try your best to follow the recommended [file structure](https://angular.io/guide/styleguide#overall-structural-guidelines) and general [style guide](https://angular.io/guide/styleguide) in the angular documentation.
