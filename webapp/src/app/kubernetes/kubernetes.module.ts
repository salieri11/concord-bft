/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule } from '@angular/common/http';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { GridModule } from '../grid/grid.module';
import { KubernetesService } from './shared/kubernetes.service';
import { KubernetesComponent } from './kubernetes.component';
import { KubernetesListComponent } from './kubernetes-list/kubernetes-list.component';
import { KubernetesFormComponent } from './kubernetes-form/kubernetes-form.component';

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
  declarations: [KubernetesComponent, KubernetesListComponent, KubernetesFormComponent],
  providers: [
    KubernetesService
  ]
})
export class KubernetesModule { }
