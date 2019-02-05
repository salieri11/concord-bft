/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MarketingComponent } from './marketing.component';
import { ClarityModule } from '@clr/angular';
import { RouterModule } from '@angular/router';
import { TranslateModule } from '@ngx-translate/core';

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    ClarityModule,
    TranslateModule,
  ],
  declarations: [MarketingComponent],
  exports: [MarketingComponent]
})
export class MarketingModule { }
