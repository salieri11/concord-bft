/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class DeployWizard {
  progressEl = '#deployProgress';
  progPerceEl = `${this.progressEl} .progress span`;

  next() {
    return element(by.css('.clr-step-button')).click();
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

  selectThirdZone() {
    return element(by.css('#clientZoneSelect option:nth-child(3)')).click();
  }

  getClientGroupName() {
    return element(by.css('#deploy-page-clients-group-0-name-input'));
  }

  selectSmallNodeSizing() {
    return element(by.css('#Small'));
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

  getHealthNumber() {
    return element(by.css('.health-number')).getText();
  }

  deploy() {
    return element(by.css('.clr-step-button.btn-primary')).click();
  }

  getPercentage(): Promise<string> {
    return element(by.css('.progress-label')).getText();
  }
}
