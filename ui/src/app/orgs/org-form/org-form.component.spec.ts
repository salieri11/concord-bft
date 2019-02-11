/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OrgFormComponent } from './org-form.component';
import { MockSharedModule } from '../../shared/shared.module';
import { OrgService } from '../shared/org.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

describe('OrgFormComponent', () => {
  let component: OrgFormComponent;
  let fixture: ComponentFixture<OrgFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        RouterTestingModule
      ],
      declarations: [ OrgFormComponent ],
      providers: [ OrgService, ErrorAlertService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OrgFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
