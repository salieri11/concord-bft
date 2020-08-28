/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { CodeHighlighterComponent } from './code-highlighter.component';
import { getSpecTestingModule } from '../../shared-testing.module';

describe('CodeHighlighterComponent', () => {
  let component: CodeHighlighterComponent;
  let fixture: ComponentFixture<CodeHighlighterComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CodeHighlighterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
