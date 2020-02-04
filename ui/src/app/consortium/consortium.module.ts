/*
 * Copyright 2018-2019 VMware, all rights reserved.
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
import { ConsortiumComponent } from './consortium.component';
import { ConsortiumListComponent } from './consortium-list/consortium-list.component';

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
  declarations: [ConsortiumComponent, ConsortiumListComponent]
})
export class ConsortiumModule { }
