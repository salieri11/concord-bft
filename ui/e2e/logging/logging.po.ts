/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class LoggingPage {
  navigateTo() {
    element(by.id('sidenav-logging')).click();
  }

  getConcordOption() {
    return element(by.css('.service-dropdown option:nth-child(1)'));
  }

  getLoadMoreButton() {
    return element(by.cssContainingText('button', 'Load More'));
  }

  getLogItemRowCount() {
    return element.all(by.css('clr-dg-row')).count();
  }

  getTimeSelectionButton() {
    return element(by.css('.time-selector'));
  }

  getSixHourTimeButton() {
    return element(by.cssContainingText('.dropdown-item', '6 Hours'));
  }

  getHeatMapXAxisLabelText() {
    return element(by.css('.heat-map-x-axis-label')).getText();
  }

  getHeatMapYAxisLabelText() {
    return element(by.css('.heat-map-y-axis-label')).getText();
  }

  getTotalCount() {
    return element(by.css('.total-count-text'));
  }

}
