/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module

import { HttpClientModule } from '@angular/common/http';
import { ClarityModule } from '@clr/angular';
import { CommonModule } from '@angular/common';
import { GridModule } from '../grid/grid.module';
import { TranslateModule } from '@ngx-translate/core';

import { OrgRoutingModule } from './org-routing.module';
import { OrgManagementComponent } from './org-management.component';
import { OrgManagementService } from './shared/org-management.service';
import { OrgListComponent } from './org-list/org-list.component';

@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    BrowserAnimationsModule,
    BrowserModule,
    HttpClientModule,
    ReactiveFormsModule,
    FormsModule,
    GridModule,
    OrgRoutingModule,
    TranslateModule,
  ],
  declarations: [
    OrgManagementComponent,
    OrgListComponent
  ],
  providers: [
    OrgManagementService
  ],
  exports: [
    OrgListComponent
  ]
})
export class OrgManagementModule { }
