/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { UsersComponent } from './users/users.component';
import { GridModule } from '../grid/grid.module';
import { UsersService } from './shared/users.service';
import { UserListComponent } from './user-list/user-list.component';
import { UserFormComponent } from './user-form/user-form.component';
import { UserSettingsComponent } from './user-settings/user-settings.component';
import { AuthenticationFormComponent } from './authentication-form/authentication-form.component';

@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    GridModule,
    TranslateModule,
    ReactiveFormsModule
  ],
  declarations: [UsersComponent, UserListComponent, UserFormComponent, UserSettingsComponent, AuthenticationFormComponent],
  providers: [UsersService]
})
export class UsersModule { }
