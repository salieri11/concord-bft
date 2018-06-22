/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { RouterTestingModule } from '@angular/router/testing';

import { SmartContractDetailContainerComponent } from './smart-contract-detail-container.component';
import { SmartContractVersionDetailsComponent } from '../smart-contract-version-details/smart-contract-version-details.component';
import { ContractPayloadPreviewModalComponent } from '../contract-payload-preview-modal/contract-payload-preview-modal.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from '../smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';

describe('SmartContractDetailContainerComponent', () => {
  let component: SmartContractDetailContainerComponent;
  let fixture: ComponentFixture<SmartContractDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        FormsModule,
        HttpClientTestingModule
      ],
      declarations: [
        SmartContractDetailContainerComponent,
        SmartContractVersionDetailsComponent,
        ContractPayloadPreviewModalComponent,
        SmartContractsSolidityFunctionInputsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractDetailContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
