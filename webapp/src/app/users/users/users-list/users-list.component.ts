/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { personaOptions, Personas } from '../../../shared/persona.service';
import { GridOptions } from '../../../grid/shared/grid.model';
import { UsersService } from '../shared/users.service';
import { GridComponent } from '../../../grid/grid.component';
import { User } from '../shared/user.model';

@Component({
  selector: 'athena-users-list',
  templateUrl: './users-list.component.html',
  styleUrls: ['./users-list.component.scss']
})
export class UsersListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('grid') grid: GridComponent;
  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  gridOptions: GridOptions = new GridOptions();
  selectedRows: Array<User>;
  user: User;
  addUserForm: FormGroup;
  editUserForm: FormGroup;
  personaOptions = personaOptions;

  constructor(private translate: TranslateService, private usersService: UsersService, private fb: FormBuilder) {
    this.gridOptions.getData = () => {
      return this.usersService.getFakeData();
    };

    this.translate.get('users.grid').subscribe(grid => this.handleGrid(grid));
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<User>): void {
    this.selectedRows = rows;
  }

  deleteUser(): void {
    this.selectedRows.forEach(user => {
      this.usersService.deleteUser(user.id);
    });
    this.openModalForm = false;
    this.grid.clearSelection();
  }

  private createEditUserForm(): void {
    this.editUserForm = this.fb.group(({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      email: ['', Validators.required],
      persona: ['', Validators.required]
    }));
    this.user = this.selectedRows[0];
    this.editUserForm.patchValue(this.user);
  }

  private createAddUserForm(): void {
    this.addUserForm = this.fb.group(({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      email: ['', Validators.required],
      persona: ['', Validators.required]
    }));
  }

  openAddUserForm(): void {
    this.openModal(('add'));
  }

  openEditUserForm(): void {
    this.openModal('edit');
  }

  confirmUserDeletion() {
    this.openModal('delete');
  }

  addUser() {
    const addUserFormModel = this.addUserForm.value;
    const date = new Date();
    const addUser: User = {
      firstName: addUserFormModel.firstName,
      lastName: addUserFormModel.lastName,
      email: addUserFormModel.email,
      persona: addUserFormModel.persona,
      createdOn: date.getDate(),
      updatedOn: date.getDate()
    };

    this.openModalForm = false;
    this.usersService.createUser(addUser);
  }

  editUser() {
    const date = new Date();
    const editUser: User = {
      id: this.user.id,
      firstName: this.editUserForm.value.firstName,
      lastName: this.editUserForm.value.lastName,
      email: this.editUserForm.value.email,
      persona: this.editUserForm.value.persona,
      createdOn: this.user.createdOn,
      updatedOn: date.getTime()
    };
    this.openModalForm = false;
    this.usersService.editUser(editUser);
    this.grid.clearSelection();
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddUserForm();
        this.modalSize = 'md';
        this.translate.get('users.addUserForm.title').subscribe(title => this.modalTitle = title);
        break;
      case 'edit':
        this.createEditUserForm();
        this.modalSize = 'md';
        this.translate.get('users.editUserForm.title').subscribe(title => this.modalTitle = title);
        break;
      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('users.deleteUserForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;
  }

  handleGrid(grid: any): void {
    this.gridOptions.paginationTitle = grid.pagination.title;
    this.gridOptions.columns = [{
      id: 'firstName',
      name: grid.columns.firstName.title,
      type: 'string'
    }, {
      id: 'lastName',
      name: grid.columns.lastName.title,
      type: 'string'
    }, {
      id: 'email',
      name: grid.columns.email.title,
      type: 'string'
    }, {
      id: 'persona',
      name: grid.columns.persona.title,
      type: 'string'
    }, {
      id: 'createdOn',
      name: grid.columns.createdOn.title,
      type: 'date'
    }, {
      id: 'updatedOn',
      name: grid.columns.updatedOn.title,
      type: 'date'
    }
    ];
  }
}
