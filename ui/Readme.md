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
- Install chrome and xvfb `sudo apt-get install xvfb chromium-browser`
- Export your VMC api key, found here (found here)[https://console.cloud.vmware.com/csp/gateway/portal/#/user/tokens] `export LINT_API_KEY=XXXXXXXXX`
- Add your fluentd api key to the `docker/fluentd/fluentd.conf` file and change the below line.

```shell
Authorization Bearer <ADD-LOGINTELLIGENCE-KEY-HERE>
```

- Now we need to extend the original docker-compose file with our fluentd config on helen 

```shell
docker-compose -f docker-compose.yml -f docker-compose-fluentd.yml up -d
```

- Instantiate a new instance `python3 reinitializeDatabases.py`
- Then `python3 main.py UiTests --noLaunch`
- If tests are failing you may need to relaunch with docker-compose

**Manually**
- Unit Tests `npm run test`
- E2E Tests `npm run e2e`
- Linter `npm run lint`

### File Structure and Coding Style
Try your best to follow the recommended [file structure](https://angular.io/guide/styleguide#overall-structural-guidelines) and general [style guide](https://angular.io/guide/styleguide) in the angular documentation.
