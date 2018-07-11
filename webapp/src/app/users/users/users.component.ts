/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { Personas } from '../../shared/persona.service';
import { UserListComponent } from '../user-list/user-list.component';
import { UserFormComponent } from '../user-form/user-form.component';
import { User } from '../shared/user.model';

@Component({
  selector: 'athena-users',
  templateUrl: './users.component.html',
  styleUrls: ['./users.component.scss']
})
export class UsersComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('usersList') usersList: UserListComponent;
  @ViewChild('usrForm') userForm: UserFormComponent;

  selected: Array<User>;

  constructor() { }

  ngOnInit() { }

  onSelectedUsersChange(rows: Array<User>) {
    this.selected = rows;
  }

  addUser(user: User) {
    this.usersList.grid.addRow(user);
  }

  editUser() {
    this.usersList.grid.reload();
  }

  deleteUsers() {
    this.usersList.grid.reload();
  }
}
