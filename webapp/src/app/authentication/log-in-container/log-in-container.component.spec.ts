/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MockTranslateModule } from '../../mocks/mock-translate.module';

import { AuthenticationService } from '../../shared/authentication.service';

import { LogInContainerComponent } from './log-in-container.component';
import { PersonaService } from '../../shared/persona.service';

describe('LogInContainerComponent', () => {
  let component: LogInContainerComponent;
  let fixture: ComponentFixture<LogInContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ReactiveFormsModule,
        MockTranslateModule
      ],
      providers: [AuthenticationService, PersonaService],
      declarations: [ LogInContainerComponent ]
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
