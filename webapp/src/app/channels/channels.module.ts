/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { ClarityModule } from '@clr/angular';
import { HttpClientModule } from '@angular/common/http';
import { GridModule } from '../grid/grid.module';

import { ChannelsComponent } from './channels.component';
import { ChannelListComponent } from './channel-list/channel-list.component';

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
  ],
  declarations: [ChannelsComponent, ChannelListComponent],
  exports: [ChannelListComponent],
})
export class ChannelsModule { }
