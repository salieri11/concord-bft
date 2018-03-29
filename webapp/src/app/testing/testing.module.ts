/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';

import { ClarityModule } from '@clr/angular';

import { TestingGroundComponent } from './testing-ground/testing-ground.component';

const routes: Routes = [
  {path: 'testing', component: TestingGroundComponent},
];

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    RouterModule.forChild(routes),
    ClarityModule
  ],
  declarations: [TestingGroundComponent]
})
export class TestingModule { }
