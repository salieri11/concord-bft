/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { SwaggerComponent } from './swagger/swagger.component';

export const swaggerRoutes: Routes = [
  { path: 'api', component: SwaggerComponent },
];
