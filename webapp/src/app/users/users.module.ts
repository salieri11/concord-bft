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
import { UsersService } from './users/shared/users.service';
import { UsersListComponent } from './users/users-list/users-list.component';

@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    GridModule,
    TranslateModule,
    ReactiveFormsModule
  ],
  declarations: [UsersComponent, UsersListComponent],
  providers: [UsersService]
})
export class UsersModule { }
