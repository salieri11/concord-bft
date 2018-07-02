/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { BlocksContainerComponent } from './blocks-container/blocks-container.component';
import { BlockDetailContainerComponent } from './block-detail-container/block-detail-container.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { TransactionsModule } from '../transactions/transactions.module';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule
  ],
  declarations: [
    BlocksContainerComponent,
    BlockDetailContainerComponent,
    BlockDetailsComponent
  ]
})
export class BlocksModule {
}
