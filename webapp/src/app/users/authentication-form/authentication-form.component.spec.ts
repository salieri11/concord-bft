/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { AuthenticationFormComponent } from './authentication-form.component';
import { MockSharedModule } from '../../shared/shared.module';

describe('AuthenticationFormComponent', () => {
  let component: AuthenticationFormComponent;
  let fixture: ComponentFixture<AuthenticationFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule
      ],
      declarations: [ AuthenticationFormComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(AuthenticationFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
