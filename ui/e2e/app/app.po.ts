/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class AppPage {

  goToConsortium() {
    return element(by.css('#go'));
  }

  goToDeployWizard() {
    const deploy = element(by.id('sidenav-deploy'));

    if (deploy.isPresent()) {
      return deploy;
    } else {
      return element(by.id('sidenav-deploy'));
    }
  }

  goToZoneList() {
    element(by.css('#sidenav-zones')).click();
  }

  goToDetailsPage() {
    element(by.css('#sidenav-details')).click();
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
