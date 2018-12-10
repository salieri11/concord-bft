/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';

import { GridComponent } from './grid.component';
import { ErrorAlertService } from '../shared/global-error-handler.service';
import { SharedModule } from '../shared/shared.module';


@NgModule({
  imports: [
    FormsModule,
    SharedModule,
    RouterModule
  ],
  declarations: [GridComponent],
  providers: [ErrorAlertService],
  exports: [
    GridComponent,
  ],
})
export class GridModule { }
