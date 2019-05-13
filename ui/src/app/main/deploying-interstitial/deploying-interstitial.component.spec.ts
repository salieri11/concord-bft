/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReactiveFormsModule } from '@angular/forms';

import { AuthenticationService } from '../../shared/authentication.service';
import { MockSharedModule } from '../../shared/shared.module';
import { PersonaService } from '../../shared/persona.service';
import { DeployingInterstialComponent } from './deploying-interstitial.component';

describe('DeployingInterstialComponent', () => {
  let component: DeployingInterstialComponent;
  let fixture: ComponentFixture<DeployingInterstialComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        HttpClientTestingModule,
        ReactiveFormsModule
      ],
      declarations: [ DeployingInterstialComponent ],
      providers: [ AuthenticationService, PersonaService ],
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DeployingInterstialComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
