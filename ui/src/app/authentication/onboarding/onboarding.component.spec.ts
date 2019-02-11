/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReactiveFormsModule } from '@angular/forms';

import { MockTranslateModule } from '../../mocks/mock-translate.module';
import { OnboardingComponent } from './onboarding.component';
import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';

describe('OnboardingComponent', () => {
  let component: OnboardingComponent;
  let fixture: ComponentFixture<OnboardingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockTranslateModule,
        HttpClientTestingModule,
        ReactiveFormsModule
      ],
      declarations: [ OnboardingComponent ],
      providers: [ AuthenticationService, PersonaService ],
    })
    .compileComponents();
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
