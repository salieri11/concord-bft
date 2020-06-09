/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element, protractor, ProtractorExpectedConditions } from 'protractor';
import { BROWSER_WAIT_TIME } from './constants';

const until = protractor.ExpectedConditions;

/**
 * Actually wait for element by id to be interactable on the page;
 * not just being present in HTML which can still be not interactable.
 * @param cdd css locator pattern
 */
export function waitInteractableFor(css: string) {
  browser.wait(until.elementToBeClickable(element(by.css(css))), BROWSER_WAIT_TIME);
}

export function waitFor(css: string) {
  browser.wait(until.presenceOf(element(by.css(css))), BROWSER_WAIT_TIME);
}

export function waitForURLContains(url: string) {
  browser.wait(until.urlContains(url), BROWSER_WAIT_TIME);
}

export function waitToDisappear(css: string) {
  browser.wait(until.invisibilityOf(element(by.css(css))), BROWSER_WAIT_TIME);
}

export function waitForText(item) {
  browser.wait(until.presenceOf(item), BROWSER_WAIT_TIME);
}
