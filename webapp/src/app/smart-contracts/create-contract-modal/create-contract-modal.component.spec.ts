/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';
import { ReactiveFormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { CreateContractModalComponent } from './create-contract-modal.component';

describe('CreateContractModalComponent', () => {
  let component: CreateContractModalComponent;
  let fixture: ComponentFixture<CreateContractModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        ReactiveFormsModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [ CreateContractModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateContractModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
