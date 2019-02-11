/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { ClrDropdown } from '@clr/angular';

import { Personas } from '../../shared/persona.service';
import { UserListComponent } from '../user-list/user-list.component';
import { UserFormComponent } from '../user-form/user-form.component';
import { User } from '../shared/user.model';
import { TourService } from '../../shared/tour.service';
import { UsersService } from '../shared/users.service';

@Component({
  selector: 'concord-users',
  templateUrl: './users.component.html',
  styleUrls: ['./users.component.scss']
})
export class UsersComponent implements OnInit, OnDestroy {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin, Personas.OrgAdmin];
  @ViewChild('usersList') usersList: UserListComponent;
  @ViewChild('userForm') userForm: UserFormComponent;
  @ViewChild('userActionsMenu') userActionsMenu: ClrDropdown;

  userActionMenuToggleChanges: Subscription;
  users: User[];
  selected: User[];

  constructor(private usersService: UsersService, private tourService: TourService) {
  }

  ngOnInit() {
    this.userActionMenuToggleChanges = this.tourService.userActionsDropdownChanges$.subscribe((openMenu) => {
      setTimeout(() => {
        this.userActionsMenu.ifOpenService.open = openMenu;
      });
    });
    this.getUsers();
  }

  ngOnDestroy() {
    this.userActionMenuToggleChanges.unsubscribe();
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
