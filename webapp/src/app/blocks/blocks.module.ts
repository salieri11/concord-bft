/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { ClarityModule } from '@clr/angular';

import { BlocksContainerComponent } from './blocks-container/blocks-container.component';
import { BlockDetailContainerComponent } from './block-detail-container/block-detail-container.component';

const routes: Routes = [
  {path: 'blocks', component: BlocksContainerComponent},
  {path: 'blocks/:id', component: BlockDetailContainerComponent}
];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    ClarityModule
  ],
  declarations: [BlocksContainerComponent, BlockDetailContainerComponent]
})
export class BlocksModule { }
