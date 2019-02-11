/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { SmartContractComponent } from './smart-contract/smart-contract.component';
import { SmartContractsComponent } from './smart-contracts/smart-contracts.component';

export const smartContractRoutes: Routes = [
  {
    path: '',
    component: SmartContractsComponent
  },
  {
    path: ':contractId',
    component: SmartContractComponent
  },
  {
    path: ':contractId/versions/:version',
    component: SmartContractComponent
  }
];
