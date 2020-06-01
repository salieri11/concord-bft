/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AppPage } from '../app/app.po';
import { Zone } from './zone.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Add Zone', () => {
  let appPage: AppPage;
  let zone: Zone;

  beforeEach(() => {
    appPage = new AppPage();
    zone = new Zone();
    browser.waitForAngularEnabled(false);
  });

  it('should add a vcenter form', () => {
    const title = 'DAML e2e';
    browser.sleep(5000);
    appPage.goToZoneList();
    browser.sleep(800);
    zone.openNewZone();
    browser.sleep(1000);
    const message = zone.fillOutVcenter();
    waitFor('#onPremSuccess');
    expect(zone.getSuccessMessage()).toBe('Connection successful to your on premises vCenter.');
  });

  it('should fill in the rest of the form add and update', () => {
    browser.sleep(100);
    zone.fillOutRest();
    zone.add();
    waitFor('#updateZone');
    zone.update();
    browser.sleep(3000);
    expect(zone.updateText()).toBe('UPDATE');
  });


  it('should reload and all data should be there', () => {
    browser.navigate().back();
    browser.sleep(2000);
    browser.navigate().forward();
    browser.sleep(2000);
    const values = zone.getValues();

    // Safely injected credentials fron Jenkins (See BC-2712 for more information)
    const zoneCreds = browser.params.credentials.zone;
    let i = 0;
    expect(values[i++]).toEqual('test');
    expect(values[i++]).toEqual('A');
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
    expect(values[i++]).toEqual('fluentd');
    expect(values[i++]).toEqual(zoneCreds.logInsight.password);
    expect(values[i++]).toEqual('https://vmware.wavefront.com');
    expect(values[i++]).toEqual(zoneCreds.wavefront.token);
    expect(values[i++]).toEqual('https://my.elk.com');
    expect(values[i++]).toEqual('TestELKUser');
    expect(values[i++]).toEqual('TestELKPass');
    expect(values[i++]).toEqual('https://vmware-docker-blockchainsaas.bintray.io');
    expect(values[i++]).toEqual(zoneCreds.bintray.username);
    expect(values[i++]).toEqual(zoneCreds.bintray.password);
    expect(values[i++]).toEqual('127.0.0.1');
    expect(values[i++]).toEqual('80');
    expect(values[i++]).toEqual('127.0.0.1');
    expect(values[i++]).toEqual('443');
  });


});
