/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule } from '@angular/router';

import { SharedModule } from '../shared/shared.module';
import { BlockListComponent } from './block-list/block-list.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { TransactionsModule } from '../transactions/transactions.module';
import { BlockComponent } from './block/block.component';
import { BlockGraphModule } from '../block-graph/block-graph.module';

@NgModule({
  imports: [
    SharedModule,
    RouterModule,
    TransactionsModule,
    BlockGraphModule
  ],
  declarations: [
    BlockListComponent,
    BlockComponent,
    BlockDetailsComponent
  ]
})
export class BlocksModule { }
