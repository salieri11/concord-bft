/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */


import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { WelcomeComponent } from './welcome.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('WelcomeComponent', () => {
  let component: WelcomeComponent;
  let fixture: ComponentFixture<WelcomeComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(WelcomeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
