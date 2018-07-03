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
import { TranslateModule } from '@ngx-translate/core';
import { HttpClientModule } from '@angular/common/http';
import { RouterModule } from '@angular/router';

import { OrgsModule } from '../orgs/orgs.module';
import { ChannelsModule } from '../channels/channels.module';
import { GridModule } from '../grid/grid.module';
import { BlockchainsComponent } from './blockchains.component';
import { BlockchainComponent } from './blockchain/blockchain.component';
import { BlockchainsListComponent } from './blockchains-list/blockchains-list.component';
import { BlockchainFormComponent } from './blockchain-form/blockchain-form.component';

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
    OrgsModule,
    ChannelsModule,
    TranslateModule,
    RouterModule
  ],
  declarations: [
    BlockchainsComponent,
    BlockchainComponent,
    BlockchainsListComponent,
    BlockchainFormComponent,
  ]
})
export class BlockchainsModule { }
