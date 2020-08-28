/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { CodeHighlighterComponent } from './code-highlighter.component';
import { testFor, prepareEach, beforeTesting } from '../../../../test.helper.spec';

describe('CodeHighlighterComponent', () => {
  const test = testFor(CodeHighlighterComponent).expedite({
    imports: [], provides: [], declarations: [CodeHighlighterComponent],
  }, beforeTesting(() => { }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
