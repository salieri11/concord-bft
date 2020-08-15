/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { SwaggerComponent } from './swagger.component';

describe('SwaggerComponent', () => {
  const test = testFor(SwaggerComponent).expedite({
    imports: [], provides: [], declarations: [SwaggerComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
