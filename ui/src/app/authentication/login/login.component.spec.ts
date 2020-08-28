/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { AuthenticationService } from '../../shared/authentication.service';
import { MockSharedModule } from '../../shared/shared.module';
import { MockTranslateModule } from '../../mocks/mock-translate.module';
import { LogInContainerComponent } from './login.component';
import { VersionComponent } from '../../shared/components/version/version.component';
import { PersonaService } from '../../shared/persona.service';


describe('LogInContainerComponent', () => {
  let component: LogInContainerComponent;
  let fixture: ComponentFixture<LogInContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ReactiveFormsModule,
        MockTranslateModule,
        MockSharedModule,
        HttpClientTestingModule
      ],
      providers: [AuthenticationService, PersonaService],
      declarations: [ LogInContainerComponent, VersionComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LogInContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
