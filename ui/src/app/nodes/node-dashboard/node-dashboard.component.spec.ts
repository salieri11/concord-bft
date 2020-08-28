/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

import { NodeDashboardComponent } from './node-dashboard.component';


describe('NodeDashboardComponent', () => {
  let component: NodeDashboardComponent;
  let fixture: ComponentFixture<NodeDashboardComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodeDashboardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
