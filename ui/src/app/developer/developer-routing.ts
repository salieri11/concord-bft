/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';
import { ApiComponent } from './api/api.component';
import { ResourcesComponent } from './resources/resources.component';


export const developerRoutes: Routes = [
  {
    path: 'apis', component: ApiComponent
  }, {
    path: 'examples', component: ResourcesComponent
  }
];
