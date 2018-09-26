/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { ClrFormsNextModule } from '@clr/angular';

import { SharedModule } from '../shared/shared.module';
import { TransactionsModule } from '../transactions/transactions.module';

import { SmartContractsComponent } from './smart-contracts/smart-contracts.component';
import { SmartContractComponent } from './smart-contract/smart-contract.component';
import { SmartContractVersionComponent } from './smart-contract-version/smart-contract-version.component';
import { ContractFormComponent } from './contract-form/contract-form.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from './smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';
import { ContractPayloadPreviewFormComponent } from './contract-payload-preview-form/contract-payload-preview-form.component';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    FormsModule,
    TourNgxPopperModule,
    ClrFormsNextModule,
    TransactionsModule
  ],
  declarations: [
    SmartContractsComponent,
    SmartContractComponent,
    SmartContractVersionComponent,
    ContractPayloadPreviewFormComponent,
    ContractFormComponent,
    SmartContractsSolidityFunctionInputsComponent
  ]
})
export class SmartContractsModule { }
