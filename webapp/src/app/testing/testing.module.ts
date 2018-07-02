/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { SharedModule } from '../shared/shared.module';
import { TestingGroundComponent } from './testing-ground/testing-ground.component';

@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    TranslateModule,
    SharedModule
  ],
  declarations: [TestingGroundComponent]
})
export class TestingModule { }
