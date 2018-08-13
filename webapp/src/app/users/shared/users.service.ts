/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/index';

import { Personas } from '../../shared/persona.service';
import { User, UserResponse } from './user.model';
import { HttpClient } from '@angular/common/http';
import { GridListResponse } from '../../grid/shared/grid.model';
import { map } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class UsersService {

  constructor(private http: HttpClient) {

  }

  path = '/api/users/';
  users: User[] = [];

  getList(): Observable<GridListResponse> {
    return this.http.get<UserResponse>(this.path).pipe(
      map(response => this.handleResponse(response)));
  }

  private handleResponse(response: UserResponse): GridListResponse {
    console.log(response);
    return {
      objects: response._embedded.users,
      meta: {
        size: response.page.size,
        total: response.page.totalElements,
        totalPages: response.page.totalPages
      }
    };
  }

  getFakeData(): Observable<GridListResponse>  {
    const d = new Date(),
      data = {
        objects: [{
          id: 1,
          name: 'Donnette Foller',
          firstName: 'Donette',
          lastName: 'Foller',
          email: 'donnett@email.com',
          persona: Personas.ConsortiumAdmin,
          organization: 'Organization 1',
          updatedOn: d.setDate(d.getDate() - 10),
          createdOn: d.setDate(d.getDate() - 10)
        }, {
          id: 2,
          name: 'Abel Dilliard',
          firstName: 'Abel',
          lastName: 'Dilliard',
          email: 'abel@email.com',
          organization: 'Organization 2',
          persona: Personas.OrgAdmin,
          updatedOn: d.setDate(d.getDate() - 10),
          createdOn: d.setDate(d.getDate() - 10)
        }, {
          id: 3,
          name: 'Sage Venere',
          firstName: 'Sage',
          lastName: 'Venere',
          email: 'sage@email.com',
          organization: 'Organization 1',
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

  createUser(user: User): Observable<any> {
    return this.http.post<User>(this.path, user);
  }

  deleteUser(userId: number): Observable<any> {
    const url = `${this.path}/${userId}`;
    return this.http.delete(url);
  }

  editUser(user: User) {
    return this.http.put<User>(this.path, user);
  }
}
