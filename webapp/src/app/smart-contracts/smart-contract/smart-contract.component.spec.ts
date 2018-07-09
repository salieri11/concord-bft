/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { RouterTestingModule } from '@angular/router/testing';

import { SmartContractComponent } from './smart-contract.component';
import { SmartContractVersionComponent } from '../smart-contract-version/smart-contract-version.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from '../smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';

describe('SmartContractComponent', () => {
  let component: SmartContractComponent;
  let fixture: ComponentFixture<SmartContractComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        FormsModule,
        HttpClientTestingModule
      ],
      declarations: [
        SmartContractComponent,
        SmartContractVersionComponent,
        ContractPayloadPreviewFormComponent,
        SmartContractsSolidityFunctionInputsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
