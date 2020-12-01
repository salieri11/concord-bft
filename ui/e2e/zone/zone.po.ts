/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';
import { waitFor } from '../helpers/utils';

const stepperExpandDelay = 1000;

export class Zone {
  progressEl = '#deployProgress';
  progPerceEl = `${this.progressEl} .progress span`;
  cert = `-----BEGIN CERTIFICATE-----
MIIDMTCCAhkCFDNALduPhMbX4lfTpo/2cDz6r9/cMA0GCSqGSIb3DQEBCwUAMGcx
CzAJBgNVBAYTAlVTMQ0wCwYDVQQIDARVdGFoMQ4wDAYDVQQHDAVQcm92bzEjMCEG
A1UECgwaQUNNRSBTaWduaW5nIEF1dGhvcml0eSBJbmMxFDASBgNVBAMMC2V4YW1w
bGUubmV0MB4XDTIwMTAyMTE3MTYyOVoXDTQ1MTAyMTE3MTYyOVowQzELMAkGA1UE
BhMCVVMxDTALBgNVBAgMBFV0YWgxDjAMBgNVBAcMBVByb3ZvMRUwEwYDVQQKDAxB
Q01FIFNlcnZpY2UwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC0WMUO
rxvbC1usjcX7f+o7MnMCJM1spHMD9qwGq5hI/rSaoCWvvWqjn4vw1jA2W/JCk6b1
IxGrKgfzKKWd0uN0M8dyNuiXntURF1SFnCEmFcnRQg7iFICYdUPTnLryeQUUe68+
wf0QQlyu4tu5M+EqWOTMI1T1TaDqtAaTPTvTKOAt1Gi3FAn4ZkJsjJZ1K2On/UtO
z21DsCJqrY4HAcVFsbDU+OoipD5QWpQ3V5VE8BfVQq6fqxhT4x/WuKgfprIfq3Hc
TtyNPpHQ6ezbksA/l3UaGAGG0QY94cmGfbwZhgehg7Jg/YtAQRKQ92rPOTaQRKZX
0m+m19QY6E3WTCzJAgMBAAEwDQYJKoZIhvcNAQELBQADggEBAEsEHtPwUzDEbBWQ
jeuQRr3ln0QCfG4zKUhlfpycBVTDk1iqX7OrFzTNlCDIoDM7dlgSDa0JvEeHEt8R
BVbFjyb/3DBVKuX04kgQ83Iq9PBuLss3GhP99HMyXE5Tnnm0mRhK/gjJUSA5UG0T
UKMXdeSgWbDyEldteJSLO6ozEyBuaFIBDV+FeuIP5y1KY7h7ASTZVP8o1juVxwX5
+/ow0JunohcBummGh4ioeC0QApoL5tNTWfcGKIZI+Vv0POeOuArznuIIYGE3BlDE
NpYrS3O1uxP+5B9AIZ6yyT1sknn/X2AfC38zkfEZDxfbF182Jc5cZ4/XeCUvn3jS
MzhxUzs=
-----END CERTIFICATE-----`;

  openNewZone() {
    return element(by.css('#newZone')).click();
  }

  fillOutVcenter() {
    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    // zone location name & designation
    element(by.css('#location')).sendKeys('test');
    element(by.css('#locationDesignation')).sendKeys('A');
    element(by.css('#nextButtonNameLocation')).click();
    browser.sleep(stepperExpandDelay);
    // zone vcenter credentials and resources
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
  }

  getSuccessMessage() {
    return element(by.css('#onPremSuccess')).getText();
  }

  fillOutRest() {
    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    // Log management
    element(by.css('#liUrl')).sendKeys('10.78.20.10');
    element(by.css('#liPort')).sendKeys('9543');
    element(by.css('#liUsername')).sendKeys(zoneCreds.logInsight.username);
    element(by.css('#liPasswor')).sendKeys(zoneCreds.logInsight.password);
    element(by.css('#nextButtonLogManagement')).click();
    browser.sleep(stepperExpandDelay);
    // Metrics management
    element(by.css('#metricsSelect')).sendKeys('wavefront');
    element(by.css('#wfUrl')).sendKeys('https://vmware.wavefront.com');
    element(by.css('#wfToken')).sendKeys(zoneCreds.wavefront.token);
    // Elasticsearch
    element(by.css('#metricsSelect')).sendKeys('elasticsearch');
    browser.sleep(stepperExpandDelay / 2);
    element(by.css('#elkUrl')).sendKeys('https://my.elk.com');
    element(by.css('#elkUsername')).sendKeys('TestELKUser');
    element(by.css('#elkPassword')).sendKeys('TestELKPass');
    element(by.css('#nextButtonMetricsManagement')).click();
    browser.sleep(stepperExpandDelay);
    // Container Registry
    element(by.css('#conUrl')).sendKeys('https://vmware-docker-blockchainsaas.bintray.io');
    element(by.css('#conUsername')).sendKeys(zoneCreds.bintray.username);
    element(by.css('#conPassword')).sendKeys(zoneCreds.bintray.password);
    element(by.css('#conCert')).sendKeys(this.cert);
    element(by.css('#nextButtonContainerRegistry')).click();
    browser.sleep(stepperExpandDelay);
    // Notary Server
    element(by.css('#notaryUrl')).sendKeys('https://notaryurl.com');
    element(by.css('#notaryCert')).sendKeys(this.cert);
    element(by.css('#notaryServer')).click();
    browser.sleep(stepperExpandDelay);
    // Outbound Proxy
    element(by.css('#opHttpHost')).sendKeys('127.0.0.1');
    element(by.css('#opHttpPort')).sendKeys('80');
    element(by.css('#opHttpsHost')).sendKeys('127.0.0.1');
    element(by.css('#opHttpsPort')).sendKeys('443');
    element(by.css('#nextButtonOutboundProxy')).click();
    browser.sleep(stepperExpandDelay);
  }

  getValues(): any[] {
    element.all(by.css('button[id^=clr-accordion-header]')).each((matched, _) => {
      matched.click();
      browser.sleep(1000);
    });
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
        element(by.css('#conCert')).getAttribute('value'),
        element(by.css('#notaryUrl')).getAttribute('value'),
        element(by.css('#notaryCert')).getAttribute('value'),
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
