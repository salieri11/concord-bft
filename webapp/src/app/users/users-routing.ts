/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Routes } from '@angular/router';

import { UsersComponent } from './users/users.component';
import { UserSettingsComponent } from "./user-settings/user-settings.component";

export const usersRoutes: Routes = [
  { path: '', component: UsersComponent },
  { path: 'settings', component: UserSettingsComponent }
];
