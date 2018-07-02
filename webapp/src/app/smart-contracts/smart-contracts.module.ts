/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { SharedModule } from '../shared/shared.module';

import { SmartContractsContainerComponent } from './smart-contracts-container/smart-contracts-container.component';
import { SmartContractDetailContainerComponent } from './smart-contract-detail-container/smart-contract-detail-container.component';
import { SmartContractVersionDetailsComponent } from './smart-contract-version-details/smart-contract-version-details.component';
import { ContractPayloadPreviewModalComponent } from './contract-payload-preview-modal/contract-payload-preview-modal.component';
import { ContractFormComponent } from './contract-form/contract-form.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from './smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    FormsModule
  ],
  declarations: [
    SmartContractsContainerComponent,
    SmartContractDetailContainerComponent,
    SmartContractVersionDetailsComponent,
    ContractPayloadPreviewModalComponent,
    ContractFormComponent,
    SmartContractsSolidityFunctionInputsComponent
  ]
})
export class SmartContractsModule { }
