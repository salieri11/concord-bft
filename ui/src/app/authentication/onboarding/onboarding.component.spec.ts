/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OnboardingComponent } from './onboarding.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('OnboardingComponent', () => {
  let component: OnboardingComponent;
  let fixture: ComponentFixture<OnboardingComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OnboardingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
