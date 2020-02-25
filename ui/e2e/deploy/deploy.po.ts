/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class DeployWizard {
  progressEl = '#deployProgress';
  progPerceEl = `${this.progressEl} .progress span`;

  next() {
    return element(by.css('.clr-wizard-btn.btn-primary')).click();
  }

  selectDaml() {
    return element(by.css('#damlEngine')).click();
  }

  consortiumTitleInput() {
    return element(by.css('#consortiumName'));
  }

  selectFourReplicas() {
    return element(by.css('#numberOfReplicas option:nth-child(1)')).click();
  }

  getTitle() {
    return element(by.css('.consortium-title')).getText();
  }

  getEngine() {
    return element(by.css('.engine')).getText();
  }

  getNumberOfReplicas() {
    return element(by.css('.number-of-replicas')).getText();
  }

  deploy() {
    return element(by.css('.clr-wizard-btn.btn-success')).click();
  }

  getPercentage(): Promise<string> {
    return element(by.css(this.progPerceEl)).getText();
  }
}
