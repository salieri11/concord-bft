/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

import { NodeSizingComponent } from './node-sizing.component';

describe('NodeSizingComponent', () => {
  let component: NodeSizingComponent;
  let fixture: ComponentFixture<NodeSizingComponent>;

  beforeEach(async(() => {
    const tester = getSpecTestingModule();
    tester.importLanguagePack();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodeSizingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
