/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { Personas } from '../../shared/persona.service';
import { UserListComponent } from '../user-list/user-list.component';
import { UserFormComponent } from '../user-form/user-form.component';
import { User } from '../shared/user.model';
import { UsersService } from '../shared/users.service';

@Component({
  selector: 'concord-users',
  templateUrl: './users.component.html',
  styleUrls: ['./users.component.scss']
})
export class UsersComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin, Personas.OrgAdmin];
  @ViewChild('usersList', { static: true }) usersList: UserListComponent;
  @ViewChild('userForm', { static: true }) userForm: UserFormComponent;

  users: User[];
  selected: User[];

  constructor(private usersService: UsersService) {
  }

  ngOnInit() {
    this.getUsers();
  }

  onSelectedUsersChange(rows: Array<User>) {
    this.selected = rows;
  }

  addUser() {
    this.getUsers();
  }

  editUser() {
    this.getUsers();
  }

  deleteUsers() {
    this.getUsers();
  }

  private getUsers() {
    this.usersService.getList().subscribe((response) => {
      this.users = response;
    });
  }

}
