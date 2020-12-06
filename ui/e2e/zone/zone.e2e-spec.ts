/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AppPage } from '../app/app.po';
import { Zone } from './zone.po';
import { waitFor, waitInteractableFor, waitToDisappear } from '../helpers/utils';

describe('concord-ui Add Zone', () => {
  let appPage: AppPage;
  let zone: Zone;
  const cert = `-----BEGIN CERTIFICATE-----
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

  beforeEach(() => {
    appPage = new AppPage();
    zone = new Zone();
    browser.waitForAngularEnabled(false);
  });

  it('should add a vcenter form', () => {
    browser.sleep(1000);
    appPage.goToZoneList();
    browser.sleep(800);
    zone.openNewZone();
    browser.sleep(1000);
    zone.fillOutVcenter();
    waitFor('#onPremSuccess');
    expect(zone.getSuccessMessage()).toBe('Connection successful to your on premises vCenter.');
    browser.sleep(1000);
    element(by.css('#nextButtonVCenter')).click();
    browser.sleep(1000);
  });

  it('should fill in the rest of the form add and update', async () => {
    browser.sleep(100);
    zone.fillOutRest();
    zone.add();
    browser.sleep(1000);
    waitFor('#newZone');
    browser.sleep(1000);
    element(by.css('#zoneListEntry')).click();
    browser.sleep(1000);
    waitFor('#updateZone');
    element(by.css('button[id^=clr-accordion-header-onPremLocation]')).click();
    browser.sleep(1000);
    element(by.css('#locationDesignation')).clear();
    element(by.css('#locationDesignation')).sendKeys('B');
    browser.sleep(1000);
    waitInteractableFor('#updateZone');
    browser.sleep(5000);
    zone.update();
    waitFor('#zoneNothingToUpdate');
    const newZoneDesig = element(by.css('#locationDesignation')).getAttribute('value');
    expect(newZoneDesig).toBe('B');
  });


  it('should reload and all data should be there', async () => {
    browser.navigate().back();
    browser.sleep(2000);
    browser.navigate().forward();
    browser.sleep(2000);
    const values = zone.getValues();

    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    let i = 0;
    expect(values[i++]).toEqual('test');
    expect(values[i++]).toEqual('B');

    expect(values[i++]).toEqual('https://vcenter.sddc-52-62-59-206.vmwarevmc.com');
    expect(values[i++]).toEqual(zoneCreds.vcenter.username);
    expect(values[i++]).toEqual(zoneCreds.vcenter.password);
    expect(values[i++]).toEqual('Compute-ResourcePool');
    expect(values[i++]).toEqual('WorkloadDatastore');
    expect(values[i++]).toEqual('HermesTesting');
    expect(values[i++]).toEqual('vmware-vpn-4b');
    expect(values[i++]).toEqual('10.70.30.1');
    expect(values[i++]).toEqual('23');
    expect(values[i++]).toEqual('1.1.1.1,1.1.1.1');
    expect(values[i++]).toEqual('1.1.1.1-1.1.1.1');

    expect(values[i++]).toEqual('10.78.20.10');
    expect(values[i++]).toEqual('9543');
    expect(values[i++]).toEqual(zoneCreds.logInsight.username);
    expect(values[i++]).toEqual(zoneCreds.logInsight.password);

    // Wavefront should be wiped out when ELK is submitted
    expect(values[i++]).toEqual('');
    expect(values[i++]).toEqual('');

    expect(values[i++]).toEqual('https://my.elk.com');
    expect(values[i++]).toEqual('TestELKUser');
    expect(values[i++]).toEqual('TestELKPass');

    expect(values[i++]).toEqual('https://vmware-docker-blockchainsaas.bintray.io');
    expect(values[i++]).toEqual(zoneCreds.bintray.username);
    expect(values[i++]).toEqual(zoneCreds.bintray.password);
    expect(values[i++]).toEqual(cert);

    expect(values[i++]).toEqual('https://notaryurl.com');
    expect(values[i++]).toEqual(cert);

    expect(values[i++]).toEqual('127.0.0.1');
    expect(values[i++]).toEqual('80');
    expect(values[i++]).toEqual('127.0.0.1');
    expect(values[i++]).toEqual('443');
  });

});
