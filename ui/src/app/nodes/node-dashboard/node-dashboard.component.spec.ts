/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

import { NodeDashboardComponent } from './node-dashboard.component';

describe('NodeDashboardComponent', () => {
  let component: NodeDashboardComponent;
  let fixture: ComponentFixture<NodeDashboardComponent>;
  const resetComponent = () => {
    fixture = TestBed.createComponent(NodeDashboardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  };

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  it('should create', () => {
    resetComponent();
    expect(component).toBeTruthy();
  });
});
