/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { WelcomeContentComponent } from './welcome-content.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';


describe('WelcomeContentComponent', () => {
  let component: WelcomeContentComponent;
  let fixture: ComponentFixture<WelcomeContentComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(WelcomeContentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
