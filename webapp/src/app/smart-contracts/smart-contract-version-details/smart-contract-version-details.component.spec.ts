/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { SmartContractVersionDetailsComponent } from './smart-contract-version-details.component';
import { ContractPayloadPreviewModalComponent } from '../contract-payload-preview-modal/contract-payload-preview-modal.component';

describe('SmartContractVersionDetailsComponent', () => {
  let component: SmartContractVersionDetailsComponent;
  let fixture: ComponentFixture<SmartContractVersionDetailsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        HttpClientTestingModule,
        FormsModule
      ],
      declarations: [ SmartContractVersionDetailsComponent, ContractPayloadPreviewModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractVersionDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
