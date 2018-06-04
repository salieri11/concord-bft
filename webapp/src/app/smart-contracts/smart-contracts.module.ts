/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';

import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';
import { SmartContractsContainerComponent } from './smart-contracts-container/smart-contracts-container.component';
import { SmartContractDetailContainerComponent } from './smart-contract-detail-container/smart-contract-detail-container.component';
import { SmartContractVersionDetailsComponent } from './smart-contract-version-details/smart-contract-version-details.component';
import { ContractPayloadPreviewModalComponent } from './contract-payload-preview-modal/contract-payload-preview-modal.component';
import { CreateContractModalComponent } from './create-contract-modal/create-contract-modal.component';

const routes: Routes = [
  {
    path: 'smart-contracts',
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', component: SmartContractsContainerComponent },
      {
        path: ':contractId',
        component: SmartContractDetailContainerComponent
      },
      {
        path: ':contractId/versions/:version',
        component: SmartContractDetailContainerComponent
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
    SmartContractsContainerComponent,
    SmartContractDetailContainerComponent,
    SmartContractVersionDetailsComponent,
    ContractPayloadPreviewModalComponent,
    CreateContractModalComponent
  ]
})
export class SmartContractsModule { }
