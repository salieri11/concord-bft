/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HttpClient } from '@angular/common/http';
import { ActivatedRoute } from '@angular/router';
import { TranslateLoader, TranslateModule, TranslateService } from '@ngx-translate/core';
import { of as observableOf } from 'rxjs';

import { UserFormComponent } from './user-form.component';
import { UsersService } from '../shared/users.service';
import { MockSharedModule } from '../../shared/shared.module';
import { GridModule } from '../../grid/grid.module';
import { HttpLoaderFactory } from '../users/users.component.spec';
import { User } from '../shared/user.model';
import { Personas } from '../../shared/persona.service';

const addUser = {
  first_name: 'Test',
  last_name: 'User',
  email: 'test@vmware.com',
  password: 'password',
  confirmPassword: 'password',
  organization: 'org',
  role: Personas.SystemsAdmin
};

const selected: User[] = [
  {
    role: Personas.SystemsAdmin,
    user_id: 9,
    last_login: 1534534154662,
    consortium:
      {
        consortium_id: 2,
        consortium_name: 'TEST_CON'
      },
    organization:
      {
        organization_id: 1,
        organization_name: 'TEST_ORG'
      },
    name: 'Test User',
    details:
      {
        last_name: 'User',
        first_name: 'Test'
      },
    email: 'test.user@vmware.com'
  },
  {
    role: Personas.SystemsAdmin,
    user_id: 10,
    last_login: 1534534154662,
    consortium:
      {
        consortium_id: 2,
        consortium_name: 'TEST_CON'
      },
    organization:
      {
        organization_id: 1,
        organization_name: 'TEST_ORG'
      },
    name: 'Test User',
    details:
      {
        last_name: 'User',
        first_name: 'Test'
      },
    email: 'test.user.2@vmware.com'
  }
];

const expectedEditUser = {
  user_id: selected[0].user_id,
  details: {
    first_name: selected[0].details.first_name,
    last_name: selected[0].details.last_name,
  },
  name: `${selected[0].details.first_name} ${selected[0].details.last_name}`,
  email: selected[0].email,
  role: selected[0].role
};

const expectedAddUser = {
  details: {
    first_name: addUser.first_name,
    last_name: addUser.last_name,
  },
  name: `${addUser.first_name} ${addUser.last_name}`,
  email: addUser.email,
  password: addUser.password,
  role: addUser.role
};

describe('UserFormComponent', () => {
  let component: UserFormComponent;
  let fixture: ComponentFixture<UserFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        GridModule,
        HttpClientTestingModule,
        BrowserAnimationsModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [ UserFormComponent ],
      providers: [
        UsersService,
        TranslateService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'test'
              ),
            },
          },
        }
        ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On open user form', () => {
    it('should set openModalForm to true', () => {
      expect(component.openModalForm).toBe(false);
      component.openAddUserForm();
      expect(component.openModalForm).toBe(true);
    });
  });

  describe('On open create user form', () => {
    it('should set the formType to add', () => {
      expect(component.formType).toBeFalsy();
      component.openAddUserForm();
      expect(component.formType).toBe('add');
    });

    it('should call createAddUserForm', () => {
      const addUserSpy = spyOn((component as any), 'createAddUserForm');
      component.openAddUserForm();
      expect(addUserSpy).toHaveBeenCalled();
    });

    it('should set the modal title', () => {
      component.openAddUserForm();
      expect(component.modalTitle).toBe('users.addUserForm.title');
    });
  });

  describe('On open edit user form', () => {
    it('should set the formType to edit', () => {
      component.selected = selected;
      expect(component.formType).toBeFalsy();
      component.openEditUserForm();
      expect(component.formType).toBe('edit');
    });

    it('should call createEditUserForm', () => {
      const editUserSpy = spyOn((component as any), 'createEditUserForm');
      component.selected = selected;
      component.openEditUserForm();
      expect(editUserSpy).toHaveBeenCalled();
    });

    it('should set the user form value based on the selected user', () => {
      component.selected = [selected[0]];
      component.openEditUserForm();
      expect(component.user).toEqual(selected[0]);
      const formValue = component.editUserForm.value;
      expect(formValue.first_name).toBe(selected[0].details.first_name);
      expect(formValue.last_name).toBe(selected[0].details.last_name);
      expect(formValue.email).toBe(selected[0].email);
      expect(formValue.role).toBe(selected[0].role);
    });

    it('should set the modal title', () => {
      component.selected = selected;
      component.openEditUserForm();
      expect(component.modalTitle).toBe('users.editUserForm.title');
    });
  });

  describe('On create user', () => {
    let createUserSpy;
    beforeEach(() => {
      createUserSpy = spyOn((component as any).usersService, 'createUser')
        .and.returnValue(observableOf({}));
      component.openAddUserForm();
      component.addUserForm.patchValue(addUser);
    });

    it('should set openModalForm to false', () => {
      expect(component.openModalForm).toBe(true);
      component.addUser();
      expect(component.openModalForm).toBe(false);
    });

    it('should call createUser on the usersService with the appropriate values', () => {
      component.addUser();

      expect(createUserSpy).toHaveBeenCalledWith(expectedAddUser);
    });

    it('should call the handleAdd function', () => {
      const handleAddSpy = spyOn((component as any), 'handleAdd');
      component.addUser();
      expect(handleAddSpy).toHaveBeenCalled();
    });
  });

  describe('On edit user', () => {
    let editUserSpy;
    beforeEach(() => {
      editUserSpy = spyOn((component as any).usersService, 'editUser')
        .and.returnValue(observableOf({}));
      component.selected = [selected[0]];
      component.openEditUserForm();
    });

    it('should set openModalForm to false', () => {
      expect(component.openModalForm).toBe(true);
      component.editUser();
      expect(component.openModalForm).toBe(false);
    });

    it('should call editUser on the usersService with the appropriate values', () => {
      component.editUser();

      expect(editUserSpy).toHaveBeenCalledWith(expectedEditUser);
    });

    it('should call the handleEdit function', () => {
      const handleEditSpy = spyOn((component as any), 'handleEdit');
      component.editUser();
      expect(handleEditSpy).toHaveBeenCalled();
    });
  });
});
