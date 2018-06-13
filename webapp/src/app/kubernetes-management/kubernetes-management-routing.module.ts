import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { KubernetesManagementComponent } from './kubernetes-management.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

const routes: Routes = [
    {
        path: 'kubernetes',
        canActivate: [AuthenticatedGuard],
        component: KubernetesManagementComponent,
       //  children: [
			    // { path: ':id', component: BlockchainComponent },
       //  ]
    },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class KubernetesRoutingModule { }
