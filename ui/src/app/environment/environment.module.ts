/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule } from '@angular/common/http';
import { TranslateModule } from '@ngx-translate/core';
import { ClarityModule } from '@clr/angular';
import { GridModule } from '../grid/grid.module';
import { EnvironmentComponent } from './environment/environment.component';

@NgModule({
  declarations: [EnvironmentComponent],
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
  ]
})
export class EnvironmentModule { }
