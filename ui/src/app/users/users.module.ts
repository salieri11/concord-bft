/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { UsersComponent } from './users/users.component';
import { GridModule } from '../grid/grid.module';
import { UsersService } from './shared/users.service';
import { UserListComponent } from './user-list/user-list.component';
import { UserFormComponent } from './user-form/user-form.component';
import { UserSettingsComponent } from './user-settings/user-settings.component';
import { SharedModule } from '../shared/shared.module';
import { CommonModule } from '@angular/common';
import { WalletFormComponent } from './wallet-form/wallet-form.component';

@NgModule({
  imports: [
    GridModule,
    SharedModule,
    TourNgxPopperModule,
    CommonModule,
    SharedModule
  ],
  declarations: [
    UsersComponent,
    UserListComponent,
    UserFormComponent,
    UserSettingsComponent,
    WalletFormComponent
  ],
  providers: [UsersService]
})
export class UsersModule { }
