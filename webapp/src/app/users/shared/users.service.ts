/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/index';

import { Personas } from '../../shared/persona.service';
import { User, UserResponse } from './user.model';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { GridListResponse } from '../../grid/shared/grid.model';
import { map } from 'rxjs/operators';

@Injectable()
export class UsersService {
  userUrl = '/api/user';
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type': 'application/json',
    // 'Authorization': 'my-auth-token'
  });
  users: User[] = [];

  constructor(private http: HttpClient) {
  }

  getList(params?: any): Observable<GridListResponse> {
    const options = { headers: this.headers };

    if (params) {
      let httpParams = new HttpParams();

      for (const prop in params) {
        if (params[prop]) {
          httpParams = httpParams.set(prop, params[prop]);
        }
      }

      options['params'] = httpParams;
    }

    return this.http.get<UserResponse>(this.userUrl, options).pipe(
      map(response => this.handleResponse(response)));
  }

  private handleResponse(response: UserResponse): GridListResponse {

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

  createUser(user: User): Observable<any> {
    return this.http.post<User>(this.userUrl, user, {headers: this.headers });
  }

  deleteUser(userId: number): Observable<any> {
    const url = `${this.userUrl}${userId}/`;
    return this.http.delete(url, { headers: this.headers });
  }

  editUser(user: User) {
    return this.http.put<User>(this.userUrl, user, {headers: this.headers });
  }
}
