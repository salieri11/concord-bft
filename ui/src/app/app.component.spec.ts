/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { AppComponent } from './app.component';
import { beforeTesting, prepareEach, testFor } from '../test.helper.spec';

describe('AppComponent', () => {
  const test = testFor(AppComponent).expedite({
    imports: [], provides: [], declarations: [AppComponent],
  }, beforeTesting(() => {}), prepareEach(() => {}));

  it('should create the app', () => {
    expect(test.component).toBeTruthy();
  });
});
