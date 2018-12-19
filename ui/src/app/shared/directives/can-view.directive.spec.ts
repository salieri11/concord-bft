/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { CanViewDirective } from './can-view.directive';

describe('CanViewDirective', () => {
  it('should create an instance', () => {
    const directive = new CanViewDirective(null, null, null);
    expect(directive).toBeTruthy();
  });
});
