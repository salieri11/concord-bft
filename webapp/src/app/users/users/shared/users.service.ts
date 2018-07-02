/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/index';

import { Personas } from '../../../shared/persona.service';
import { User } from './user.model';

@Injectable()
export class UsersService {
  users: User[] = [];

  constructor() {
  }

  getFakeData() {
    const d = new Date(),
      data = {
        objects: [{
          id: 1,
          firstName: 'Donette',
          lastName: 'Foller',
          email: 'donnett@email.com',
          persona: Personas.ConsortiumAdmin,
          updatedOn: d.setDate(d.getDate() - 10),
          createdOn: d.setDate(d.getDate() - 10)
        }, {
          id: 2,
          firstName: 'Abel',
          lastName: 'Dilliard',
          email: 'abel@email.com',
          persona: Personas.OrgAdmin,
          updatedOn: d.setDate(d.getDate() - 10),
          createdOn: d.setDate(d.getDate() - 10)
        }, {
          id: 3,
          firstName: 'Sage',
          lastName: 'Venere',
          email: 'sage@email.com',
          persona: Personas.SystemsAdmin,
          updatedOn: d.setDate(d.getDate() - 10),
          createdOn: d.setDate(d.getDate() - 10)
        }],
        meta: {
          size: 10,
          total: 10,
          totalPages: 10
        }
      };

    this.users = data.objects;
    return new Observable(observer => {
      observer.next(data);
      observer.complete();
    });
  }

  createUser(user: User) {
    const id = this.users[this.users.length - 1].id;
    user.id = id + 1;
    this.users.push(user);
  }

  deleteUser(userId: number) {
    this.users.forEach(userObj => {
      if (userObj.id === userId) {
        this.users.splice(this.users.indexOf(userObj), 1);
      }
    });
  }

  editUser(user: User) {
    this.users.forEach(userObj => {
      if (userObj.id === user.id) {
        this.users[this.users.indexOf(userObj)] = user;
      }
    });
  }
}
