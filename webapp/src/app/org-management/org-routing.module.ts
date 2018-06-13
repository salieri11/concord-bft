import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { OrgManagementComponent } from '../org-management/org-management.component';
import { AuthenticatedGuard } from '../shared/authenticated-guard.service';

const routes: Routes = [
    {
        path: 'organization',
        canActivate: [AuthenticatedGuard],
        component: OrgManagementComponent
       //  children: [
			    // { path: 'organization', component: OrgManagementComponent },
       //  ]
    },

];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class OrgRoutingModule { }
