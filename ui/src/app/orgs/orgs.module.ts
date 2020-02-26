/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { RouterModule } from '@angular/router';
import { SharedModule } from '../shared/shared.module';

import { HttpClientModule } from '@angular/common/http';
import { ClarityModule } from '@clr/angular';
import { CommonModule } from '@angular/common';
import { GridModule } from '../grid/grid.module';
import { TranslateModule } from '@ngx-translate/core';

import { OrgsComponent } from './orgs.component';
import { OrgService } from './shared/org.service';
import { OrgListComponent } from './org-list/org-list.component';
import { InivteUserComponent } from './inivte-user/inivte-user.component';

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
    RouterModule,
    TranslateModule,
    SharedModule.forRoot()
  ],
  declarations: [
    OrgsComponent,
    OrgListComponent,
    InivteUserComponent,
  ],
  providers: [
    OrgService
  ],
  exports: [
    OrgListComponent
  ]
})
export class OrgsModule {
}