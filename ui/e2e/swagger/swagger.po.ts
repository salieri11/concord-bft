/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element } from 'protractor';

export class SwaggerPage {
  navigateTo() {
    element(by.id('sidenav-apis')).click();
  }

  getPageTitle() {
    return element(by.css('h2.title')).getText();
  }

  getOpBlockBodyForId(id) {
    return element(by.css(`${id} .opblock-body`));
  }

  getOpBlockExecuteButtonForId(id) {
    return element(by.css(`${id} .btn.execute.opblock-control__btn`));
  }

  clickSwaggerHeaderForId(id) {
    element(by.css(`${id} .opblock-summary`)).click();
  }

  clickTryItOutButtonForId(id) {
    element(by.css(`${id} .btn.try-out__btn`)).click();
  }

  clickCancelTryItOutButtonForId(id) {
    element(by.css(`${id} .btn.try-out__btn.cancel`)).click();
  }

  clickOpBlockExecuteButtonForId(id) {
    element(by.css(`${id} .btn.execute.opblock-control__btn`)).click();
  }

  getResponseForId(id) {
    return element.all(by.css(`${id} .response`)).get(0);
  }

  getResponseStatusForId(id) {
    return this.getResponseForId(id).all(by.css('.col.response-col_status')).get(0).getText();
  }

  getResponseTextForId(id) {
    return this.getResponseForId(id).all(by.css('.col.response-col_description')).get(0).getText();
  }

  getInputForAttribute(attr) {
    return element(by.css(`tr[data-param-name="${attr}"] input`));
  }

  getRequestUrlFromResponse() {
    return element(by.css('.request-url pre')).getText();
  }
}
