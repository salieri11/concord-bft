import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { BlockchainsComponent } from './blockchains.component';
import { BlockchainComponent } from './blockchain/blockchain.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

const routes: Routes = [
  {
    path: 'blockchains',
    canActivate: [AuthenticatedGuard],
    component: BlockchainsComponent,
  },
  {
    canActivate: [AuthenticatedGuard],
    path: 'blockchains/:id',
    component: BlockchainComponent
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class BlockchainsRoutingModule { }
