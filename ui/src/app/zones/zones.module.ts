/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { HttpClientModule } from '@angular/common/http';

import { SharedModule } from '../shared/shared.module';

import { ZoneFormComponent } from './zone-form/zone-form.component';
import { ZonesComponent } from './zones.component';
import { ZoneListComponent } from './zone-list/zone-list.component';
import { ZoneComponent } from './zone/zone.component';


@NgModule({
  declarations: [
    ZonesComponent,
    ZoneListComponent,
    ZoneComponent,
    ZoneFormComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    RouterModule,
    HttpClientModule
  ],
  exports: [ZonesComponent]
})
export class ZonesModule { }
