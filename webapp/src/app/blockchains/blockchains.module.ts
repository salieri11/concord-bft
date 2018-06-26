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

import { OrgManagementModule } from '../org-management/org-management.module';
import { ChannelsModule } from '../channels/channels.module';
import { GridModule } from '../grid/grid.module';
import { BlockchainsComponent } from './blockchains.component';
import { BlockchainsService } from './shared/blockchains.service';
import { KubernetesService } from '../kubernetes-management/shared/kubernetes.service';
import { BlockchainComponent } from './blockchain/blockchain.component';

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
    OrgManagementModule,
    ChannelsModule,
    TranslateModule,
    RouterModule
  ],
  declarations: [
    BlockchainsComponent,
    BlockchainComponent,
  ],
  providers: [BlockchainsService, KubernetesService]
})
export class BlockchainsModule { }
