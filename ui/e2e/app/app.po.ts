/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class AppPage {

  goToConsortium() {
    return element(by.css('#go'));
  }

  goToDeployWizard() {
    const deploy = element(by.css('#deploy'));

    if (deploy.isPresent()) {
      return deploy;
    } else {
      return element(by.css('#sidebarDeploy'));
    }
  }

  goToZoneList() {
    element(by.css('#zones')).click();
  }

  getTourTitle() {
    return element(by.css('.ngxp-title'));
  }

  getTourNextButton() {
    return element(by.css('.ngxp-btn.btn-next'));
  }

  clickTourEndButton() {
    element(by.css('.ngxp-btn.btn-end')).click();
  }
}
