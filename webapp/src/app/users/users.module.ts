/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { TranslateModule } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { UserComponent } from './user/user.component';
import { GridModule } from '../grid/grid.module';
import { UsersService } from './shared/users.service';
import { UserListComponent } from './user-list/user-list.component';
import { UserFormComponent } from './user-form/user-form.component';

@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    GridModule,
    TranslateModule,
    ReactiveFormsModule
  ],
  declarations: [UserComponent, UserListComponent, UserFormComponent],
  providers: [UsersService]
})
export class UsersModule { }
