/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { ZoneListComponent } from './zone-list/zone-list.component';
import { ZoneComponent } from './zone/zone.component';

export const zonesRoutes: Routes = [
  { path: '', component: ZoneListComponent },
  { path: ':zoneId', component: ZoneComponent }
];

