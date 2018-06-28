/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { SmartContractsComponent } from './smart-contracts/smart-contracts.component';
import { SmartContractComponent } from './smart-contract/smart-contract.component';
import { SmartContractVersionComponent } from './smart-contract-version/smart-contract-version.component';
import { ContractFormComponent } from './contract-form/contract-form.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from './smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';
import { ContractPayloadPreviewFormComponent } from './contract-payload-preview-form/contract-payload-preview-form.component';

const routes: Routes = [
  {
    path: 'smart-contracts',
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', component: SmartContractsComponent },
      {
        path: ':contractId',
        component: SmartContractComponent
      },
      {
        path: ':contractId/versions/:version',
        component: SmartContractComponent
      },
    ]
  }];

@NgModule({
  imports: [
    SharedModule,
    RouterModule.forChild(routes),
    FormsModule
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
