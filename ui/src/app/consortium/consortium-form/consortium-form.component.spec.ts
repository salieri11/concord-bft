/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ConsortiumFormComponent } from './consortium-form.component';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { ConsortiumService } from '../shared/consortium.service';

describe('ConsortiumFormComponent', () => {
  let component: ConsortiumFormComponent;
  let fixture: ComponentFixture<ConsortiumFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        RouterTestingModule
      ],
      declarations: [ ConsortiumFormComponent ],
      providers: [ ConsortiumService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConsortiumFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
