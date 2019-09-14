/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { SharedModule } from '../shared/shared.module';

import { OnPremisesFormComponent } from './on-premises-form/on-premises-form.component';
import { OnPremisesModalComponent } from './on-premises-modal/on-premises-modal.component';
import { BlockchainWizardComponent } from './blockchain-wizard/blockchain-wizard.component';
import { BlockchainService, BlockchainResolver } from './shared/blockchain.service';

@NgModule({
  declarations: [
    OnPremisesFormComponent,
    OnPremisesModalComponent,
    BlockchainWizardComponent,
  ],
  imports: [
    SharedModule
  ],
  exports: [
    OnPremisesModalComponent,
    OnPremisesFormComponent,
    BlockchainWizardComponent
  ],
  providers: [
    BlockchainService,
    BlockchainResolver,
  ]
})
export class BlockchainModule { }
