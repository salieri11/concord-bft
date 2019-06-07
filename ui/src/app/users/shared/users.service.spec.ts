/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { UsersService } from './users.service';
import { Personas } from '../../shared/persona.service';

const payload = {
  details: {
    first_name: 'Test',
    last_name: 'User',
  },
  name: 'Test User',
  email: 'test@vmware.com',
  password: 'password',
  role: Personas.SystemsAdmin
};

describe('UsersService', () => {
  let service: UsersService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        HttpClientTestingModule
      ],
      providers: [
        UsersService,
      ]
    });

    service = TestBed.get(UsersService);
  });

  it('should be created', inject([UsersService], (usersService: UsersService) => {
    expect(usersService).toBeTruthy();
  }));

  it('should fetch all users', () => {
    const httpSpy = spyOn((service as any).http, 'get');
    service.getList();
    expect(httpSpy).toHaveBeenCalledWith('api/users');
  });

  it('should post a user with a given payload', () => {
    const httpSpy = spyOn((service as any).http, 'post');
    service.createUser(payload);
    expect(httpSpy).toHaveBeenCalledWith('api/users', payload);
  });

  it('should edit a user with a given id and payload', () => {
    const httpSpy = spyOn((service as any).http, 'patch');
    (payload as any).user_id = 1;
    service.editUser(payload);
    expect(httpSpy).toHaveBeenCalledWith(`api/users/${(payload as any).user_id}`, payload);
  });

  it('should delete a user with a given id', () => {
    const userId = 1;
    const httpSpy = spyOn((service as any).http, 'delete');
    service.deleteUser(userId);
    expect(httpSpy).toHaveBeenCalledWith(`api/users/${userId}`);
  });
});
