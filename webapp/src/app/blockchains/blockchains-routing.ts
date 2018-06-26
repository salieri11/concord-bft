/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { BlockchainsComponent } from './blockchains.component';
import { BlockchainComponent } from './blockchain/blockchain.component';

export const blockChainsRoutes: Routes = [
  { path: '', component: BlockchainsComponent },
  { path: ':id', component: BlockchainComponent },
];
