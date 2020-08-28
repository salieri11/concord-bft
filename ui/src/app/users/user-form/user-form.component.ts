/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';
import { ActivatedRoute } from '@angular/router';

import { User } from '../shared/user.model';
import { UsersService } from '../shared/users.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { matchPasswordValidator } from '../../shared/custom-validators';

@Component({
  selector: 'concord-user-form',
  templateUrl: './user-form.component.html',
  styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {
  static rolesAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin, Personas.OrgAdmin];
  @Input('selected') selected: Array<User>;
  @Output('createUser') createUser: EventEmitter<any> = new EventEmitter<any>();
  @Output('updateUser') updateUser: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  user: User;
  addUserForm: FormGroup;
  editUserForm: FormGroup;
  personaOptions = PersonaService.getOptions();

  constructor(
    private translate: TranslateService,
    private usersService: UsersService,
    private fb: FormBuilder,
    private route: ActivatedRoute
    ) {
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'add':
          this.openAddUserForm();
          break;
        default:
          // code...
          break;
      }
    });
  }

  private createEditUserForm(): void {
    this.editUserForm = this.fb.group(({
      first_name: ['', Validators.required],
      last_name: ['', Validators.required],
      email: ['', Validators.required],
      role: ['', Validators.required]
    }));
    this.user = this.selected[0];
    this.editUserForm.patchValue({
      first_name: this.user.details.first_name,
      last_name: this.user.details.last_name,
      email: this.user.email,
      role: this.user.role
    });
  }

  private createAddUserForm(): void {
    this.addUserForm = this.fb.group(({
      first_name: ['', Validators.required],
      last_name: ['', Validators.required],
      email: ['', Validators.required],
      password: ['', [Validators.required, Validators.minLength(8)]],
      confirmPassword: ['', [Validators.required, Validators.minLength(8), matchPasswordValidator('password')]],
      organization: ['ADMIN', Validators.required],
      role: ['', Validators.required]
    }));
  }

  openAddUserForm(): void {
    this.openModal(('add'));
  }

  openEditUserForm(): void {
    this.openModal('edit');
  }

  addUser() {
    const addUserFormModel = this.addUserForm.value;
    const addUser: User = {
      details: {
        first_name: addUserFormModel.first_name,
        last_name: addUserFormModel.last_name,
      },
      name: `${addUserFormModel.first_name} ${addUserFormModel.last_name}`,
      email: addUserFormModel.email,
      password: addUserFormModel.password,
      role: addUserFormModel.role
    };

    this.openModalForm = false;
    this.usersService.createUser(addUser)
      .subscribe(response => this.handleAdd(response));
  }

  editUser() {
    const editUser: User = {
      user_id: this.user.user_id,
      details: {
        first_name: this.editUserForm.value.first_name,
        last_name: this.editUserForm.value.last_name,
      },
      name: `${this.editUserForm.value.first_name} ${this.editUserForm.value.last_name}`,
      email: this.editUserForm.value.email,
      role: this.editUserForm.value.role
    };
    this.openModalForm = false;
    this.usersService.editUser(editUser)
      .subscribe(response => this.handleEdit(response));
  }

  private handleAdd(response): void {
    this.openModalForm = false;
    this.createUser.emit(response);
  }

  private handleEdit(response): void {
    this.openModalForm = false;
    this.updateUser.emit(response);
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddUserForm();
        this.modalSize = 'md';
        this.modalTitle = this.translate.instant('users.addUserForm.title');
        break;
      case 'edit':
        this.createEditUserForm();
        this.modalSize = 'md';
        this.modalTitle = this.translate.instant('users.editUserForm.title');
        break;
    }

    this.openModalForm = true;
  }

}
