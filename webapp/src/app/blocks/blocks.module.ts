/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

import { BlocksContainerComponent } from './blocks-container/blocks-container.component';
import { BlockDetailContainerComponent } from './block-detail-container/block-detail-container.component';

const routes: Routes = [
  {
    path: 'blocks',
    canActivateChild: [AuthenticatedGuard],
    children: [
      { path: '', component: BlocksContainerComponent },
      { path: ':blockNumber', component: BlockDetailContainerComponent }
    ]
  }
];

@NgModule({
  imports: [
    SharedModule,
    RouterModule.forChild(routes)
  ],
  declarations: [BlocksContainerComponent, BlockDetailContainerComponent]
})
export class BlocksModule { }
