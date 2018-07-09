/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { User } from '../shared/user.model';
import { UsersService } from '../shared/users.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { matchPasswordValidator } from "../shared/custom-validators";

@Component({
  selector: 'athena-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @Input('selected') selected: Array<User>;
  @Output('createUser') createUser: EventEmitter<any> = new EventEmitter<any>();
  @Output('deleteUsers') deleteUsers: EventEmitter<any> = new EventEmitter<any>();
  @Output('updateUser') updateUser: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  user: User;
  addUserForm: FormGroup;
  editUserForm: FormGroup;
  personaOptions = PersonaService.getOptions();

  constructor(private translate: TranslateService, private usersService: UsersService, private fb: FormBuilder) {
  }

  ngOnInit() {
  }

  deleteUser(): void {
    this.selected.forEach(user => {
      this.usersService.deleteUser(user.id)
        .subscribe(response => this.handleDeletion(response));
    });
  }

  private createEditUserForm(): void {
    this.editUserForm = this.fb.group(({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      email: ['', Validators.required],
      persona: ['', Validators.required]
    }));
    this.user = this.selected[0];
    this.editUserForm.patchValue(this.user);
  }

  private createAddUserForm(): void {
    this.addUserForm = this.fb.group(({
      firstName: ['', Validators.required],
      lastName: ['', Validators.required],
      email: ['', Validators.required],
      password: ['', [Validators.required, Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, Validators.minLength(8), matchPasswordValidator('password')]],
      organization: ['', Validators.required],
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
      password: addUserFormModel.password,
      persona: addUserFormModel.persona,
      createdOn: date.getDate(),
      updatedOn: date.getDate()
    };

    this.openModalForm = false;
    this.usersService.createUser(addUser)
      .subscribe(response => this.handleAdd(response));
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
    this.usersService.editUser(editUser)
      .subscribe(response => this.handleEdit(response));
  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.openModalForm = false;
    this.createUser.emit(response);
  }

  private handleEdit(response): void {
    console.log('add response', response);
    this.openModalForm = false;
    this.updateUser.emit(response);
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.openModalForm = false;
    this.deleteUsers.emit();
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

}
