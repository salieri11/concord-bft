/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, by, element, protractor, ProtractorExpectedConditions } from 'protractor';
import { BROWSER_WAIT_TIME } from './constants';

const until = protractor.ExpectedConditions;

/**
 * Actually wait for the element to be interactable on the page;
 * not just being present in HTML which can still be not interactable.
 * @param css locator pattern
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

export const uuidv4StrictRegExp // Strict uuidv4 with 4 & 89AB restriction
  = /^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$/i;

export const uuidLaxRegExp // Free bytes hex uuid
  = /^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$/i;

export async function textContentOf(id: string): Promise<string> {
  const found = await browser.executeScript((`return document.getElementById('${id}');`));
  if (found) {
    return element(by.id(id)).getText();
  } else {
    return null;
  }
}
