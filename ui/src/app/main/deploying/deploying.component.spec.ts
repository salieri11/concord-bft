/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { DeployingComponent } from './deploying.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('DeployingComponent', () => {
  let component: DeployingComponent;
  let fixture: ComponentFixture<DeployingComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DeployingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
