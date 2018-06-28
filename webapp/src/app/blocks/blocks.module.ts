/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { BlocksListComponent } from './blocks-list/blocks-list.component';
import { BlockDetailsComponent } from './block-details/block-details.component';
import { TransactionsModule } from '../transactions/transactions.module';
import { BlockComponent } from './block/block.component';

const routes: Routes = [
  {
    path: 'blocks',
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', component: BlocksListComponent },
      { path: ':blockNumber', component: BlockComponent }
    ]
  }

];

@NgModule({
  imports: [
    SharedModule,
    TransactionsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [
    BlocksListComponent,
    BlockComponent,
    BlockDetailsComponent
    ]
})
export class BlocksModule { }
