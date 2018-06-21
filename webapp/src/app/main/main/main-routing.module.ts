/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from "@angular/router";
import { MainComponent } from "./main.component";

const routes: Routes = [
  {
   // path: '', redirectTo: 'dashboard', pathMatch: 'full'
   // path: '', component: MainComponent, children: [

   /*   {path: 'dashboard', redirectTo: 'dashboard', pathMatch: 'full'},
      {path: 'nodes', redirectTo: 'nodes', pathMatch: 'full'},
      {path: 'blocks', redirectTo: 'blocks', pathMatch: 'full'},
      {path: 'testing', redirectTo: 'testing', pathMatch: 'full'}

    ] */
  }
];

@NgModule({
  imports: [
    RouterModule.forChild(routes)
  ],
  exports: [RouterModule]
})
export class MainRoutingModule {
}
