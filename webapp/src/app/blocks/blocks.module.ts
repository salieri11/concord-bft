/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { BlocksListComponent } from './blocks-list/blocks-list.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { TransactionsModule } from '../transactions/transactions.module';
import { BlockComponent } from './block/block.component';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule
  ],
  declarations: [
    BlocksListComponent,
    BlockComponent,
    BlockDetailsComponent
  ]
})
export class BlocksModule { }
