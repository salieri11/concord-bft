/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, TestBed, ComponentFixture } from '@angular/core/testing';
import { NodesComponent } from './nodes.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('NodesComponent', () => {
  let component: NodesComponent;
  let fixture: ComponentFixture<NodesComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
