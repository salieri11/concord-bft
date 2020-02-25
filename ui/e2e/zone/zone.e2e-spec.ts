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

    expect(values[0]).toEqual('test');
    expect(values[1]).toEqual('A');
    expect(values[2]).toEqual('https://vcenter.sddc-52-62-59-206.vmwarevmc.com');
    expect(values[3]).toEqual('cloudadmin@vmc.local');
    expect(values[4]).toEqual('o8Sa*SsI!rITsR9');
    expect(values[5]).toEqual('Compute-ResourcePool');
    expect(values[6]).toEqual('WorkloadDatastore');
    expect(values[7]).toEqual('HermesTesting');
    expect(values[8]).toEqual('vmware-vpn-4b');
    expect(values[9]).toEqual('10.70.30.1');
    expect(values[10]).toEqual('23');
    expect(values[11]).toEqual('1.1.1.1;1.1.1.1');
    expect(values[12]).toEqual('1.1.1.1-1.1.1.1');
    expect(values[13]).toEqual('10.78.20.10');
    expect(values[14]).toEqual('9543');
    expect(values[15]).toEqual('fluentd');
    expect(values[16]).toEqual('Fluentd!23');
    expect(values[17]).toEqual('https://vmware.wavefront.com');
    expect(values[18]).toEqual('fbb8e2ec-7efa-4186-9c23-16d551e38595');
    expect(values[19]).toEqual('https://vmware-docker-blockchainsaas.bintray.io');
    expect(values[20]).toEqual('vmbc-asx-reader@vmware');
    expect(values[21]).toEqual('4dc71692db4a46941cedaf83d48a237e1a4cc1df');
    expect(values[22]).toEqual('127.0.0.1');
    expect(values[23]).toEqual('80');
    expect(values[24]).toEqual('127.0.0.1');
    expect(values[25]).toEqual('443');
  });


});
