/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { SharedModule } from '../shared/shared.module';

import { BlockchainWizardComponent } from './blockchain-wizard/blockchain-wizard.component';
import { BlockchainService } from './shared/blockchain.service';
import { BlockchainResolver } from './shared/blockchain.resolver';

@NgModule({
  declarations: [
    BlockchainWizardComponent,
  ],
  imports: [
    SharedModule
  ],
  exports: [
    BlockchainWizardComponent
  ],
  providers: [
    BlockchainService,
    BlockchainResolver,
  ]
})
export class BlockchainModule { }
