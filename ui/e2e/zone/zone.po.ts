/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class Zone {
  progressEl = '#deployProgress';
  progPerceEl = `${this.progressEl} .progress span`;

  openNewZone() {
    return element(by.css('#newZone')).click();
  }

  fillOutVcenter() {
    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    // zone location
    element(by.css('#location')).sendKeys('test');
    // zone designation
    element(by.css('#locationDesignation')).sendKeys('A');
    // zone designation
    element(by.css('#vcUrl')).sendKeys('https://vcenter.sddc-52-62-59-206.vmwarevmc.com');
    element(by.css('#vcUsername')).sendKeys(zoneCreds.vcenter.username);
    element(by.css('#vcPassword')).sendKeys(zoneCreds.vcenter.password);
    element(by.css('#vcRp')).sendKeys('Compute-ResourcePool');
    element(by.css('#vcStorage')).sendKeys('WorkloadDatastore');
    element(by.css('#vcFolder')).sendKeys('HermesTesting');
    element(by.css('#netName')).sendKeys('vmware-vpn-4b');
    element(by.css('#netGateway')).sendKeys('10.70.30.1');
    element(by.css('#netSubnet')).sendKeys('23');
    element(by.css('#netNS')).sendKeys('1.1.1.1,1.1.1.1');
    element(by.css('#netPool')).sendKeys('1.1.1.1-1.1.1.1');
    element(by.css('#liUrl')).sendKeys('10.78.20.10');
  }

  getSuccessMessage() {
    return element(by.css('#onPremSuccess')).getText();
  }

  fillOutRest() {
    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    element(by.css('#liPort')).sendKeys('9543');
    element(by.css('#liUsername')).sendKeys('fluentd');
    element(by.css('#liPasswor')).sendKeys(zoneCreds.logInsight.password);
    element(by.css('#wfUrl')).sendKeys('https://vmware.wavefront.com');
    element(by.css('#wfToken')).sendKeys(zoneCreds.wavefront.token);
    element(by.css('#elkUrl')).sendKeys('https://my.elk.com');
    element(by.css('#elkUsername')).sendKeys('TestELKUser');
    element(by.css('#elkPassword')).sendKeys('TestELKPass');
    element(by.css('#conUrl')).sendKeys('https://vmware-docker-blockchainsaas.bintray.io');
    element(by.css('#conUsername')).sendKeys(zoneCreds.bintray.username);
    element(by.css('#conPassword')).sendKeys(zoneCreds.bintray.password);
    element(by.css('#opHttpHost')).sendKeys('127.0.0.1');
    element(by.css('#opHttpPort')).sendKeys('80');
    element(by.css('#opHttpsHost')).sendKeys('127.0.0.1');
    element(by.css('#opHttpsPort')).sendKeys('443');
  }

  getValues(): any[] {
    return [
        element(by.css('#location')).getAttribute('value'),
        element(by.css('#locationDesignation')).getAttribute('value'),
        element(by.css('#vcUrl')).getAttribute('value'),
        element(by.css('#vcUsername')).getAttribute('value'),
        element(by.css('#vcPassword')).getAttribute('value'),
        element(by.css('#vcRp')).getAttribute('value'),
        element(by.css('#vcStorage')).getAttribute('value'),
        element(by.css('#vcFolder')).getAttribute('value'),
        element(by.css('#netName')).getAttribute('value'),
        element(by.css('#netGateway')).getAttribute('value'),
        element(by.css('#netSubnet')).getAttribute('value'),
        element(by.css('#netNS')).getAttribute('value'),
        element(by.css('#netPool')).getAttribute('value'),
        element(by.css('#liUrl')).getAttribute('value'),
        element(by.css('#liPort')).getAttribute('value'),
        element(by.css('#liUsername')).getAttribute('value'),
        element(by.css('#liPasswor')).getAttribute('value'),
        element(by.css('#wfUrl')).getAttribute('value'),
        element(by.css('#wfToken')).getAttribute('value'),
        element(by.css('#elkUrl')).getAttribute('value'),
        element(by.css('#elkUsername')).getAttribute('value'),
        element(by.css('#elkPassword')).getAttribute('value'),
        element(by.css('#conUrl')).getAttribute('value'),
        element(by.css('#conUsername')).getAttribute('value'),
        element(by.css('#conPassword')).getAttribute('value'),
        element(by.css('#opHttpHost')).getAttribute('value'),
        element(by.css('#opHttpPort')).getAttribute('value'),
        element(by.css('#opHttpsHost')).getAttribute('value'),
        element(by.css('#opHttpsPort')).getAttribute('value')
    ];
  }

  add() {
    element(by.css('#addZone')).click();
  }

  update() {
    element(by.css('#updateZone')).click();
  }

  updateText() {
    return element(by.css('#updateZone')).getText();
  }


}
