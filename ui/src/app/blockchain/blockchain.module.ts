/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { SharedModule } from '../shared/shared.module';

import { BlockchainWizardComponent } from './blockchain-wizard/blockchain-wizard.component';
import { BlockchainService } from './shared/blockchain.service';
import { BlockchainResolver } from './shared/blockchain.resolver';
import { NodeSizingComponent } from './node-sizing/node-sizing.component';
import { DeployStepperComponent } from './deploy-stepper/deploy-stepper.component';

@NgModule({
  declarations: [
    BlockchainWizardComponent,
    NodeSizingComponent,
    DeployStepperComponent,
  ],
  imports: [
    SharedModule
  ],
  exports: [
    BlockchainWizardComponent,
    DeployStepperComponent
  ],
  providers: [
    BlockchainService,
    BlockchainResolver,
  ]
})
export class BlockchainModule { }
