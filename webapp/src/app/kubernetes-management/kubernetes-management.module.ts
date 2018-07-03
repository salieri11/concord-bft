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
import { TranslateModule } from '@ngx-translate/core';

import { GridModule } from '../grid/grid.module';
import { KubernetesManagementComponent } from './kubernetes-management.component';

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
    TranslateModule
  ],
  declarations: [KubernetesManagementComponent]
})
export class KubernetesManagementModule { }
