/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export const customMatchers = {
  toHaveRendered: (util: any) => {
    return {
      compare: (el: any, selector: string) => {
        if (!util) {
          return;
        }

        if (!el || typeof el.querySelector === 'undefined') {
          return {
            pass: false,
            message: 'ERROR: Dom element is invalid.'
          };
        }

        if (selector) {
          const pass = !!el.querySelector(selector);
          const message = pass ?
            `Expected element with selector "${selector}" NOT to have been rendered, but it was.` :
            `Expected element with selector "${selector}" to have been rendered, but it was not.`;

          return { pass, message };
        } else if (selector === '') {
          return {
            pass: false,
            message: 'ERROR: Selector can not be empty.'
          };
        } else {
          return {
            pass: false,
            message: 'ERROR: No selector given.'
          };
        }

      }
    };
  }
};
