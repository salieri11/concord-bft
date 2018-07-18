/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { ClrDropdown } from '@clr/angular';

import { Personas } from '../../shared/persona.service';
import { UserListComponent } from '../user-list/user-list.component';
import { UserFormComponent } from '../user-form/user-form.component';
import { User } from '../shared/user.model';
import { TourService } from '../../shared/tour.service';

@Component({
  selector: 'athena-users',
  templateUrl: './users.component.html',
  styleUrls: ['./users.component.scss']
})
export class UsersComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin, Personas.OrgAdmin];
  @ViewChild('usersList') usersList: UserListComponent;
  @ViewChild('userForm') userForm: UserFormComponent;
  @ViewChild('userActionsMenu') userActionsMenu: ClrDropdown;

  selected: Array<User>;

  constructor(private tourService: TourService) {
  }

  ngOnInit() {
  }

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

  onNext() {
    this.tourService.toggleUserProfileMenu();
  }

}
