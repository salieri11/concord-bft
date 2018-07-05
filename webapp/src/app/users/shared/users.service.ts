/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { Observable } from 'rxjs/index';

import { Personas } from '../../shared/persona.service';
import { User, UserResponse } from './user.model';
import { HttpClient } from '@angular/common/http';
import { GridListResponse } from '../../grid/shared/grid.model';
import { map } from 'rxjs/operators';
import { ANDES_API_PREFIX } from '../../shared/shared.config';
import { AndesApi } from '../../shared/andes-api';

@Injectable({
  providedIn: 'root'
})
export class UsersService extends AndesApi {

  constructor(private http: HttpClient, @Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  users: User[] = [];

  get apiSubPath() {
    return 'user';
  }

  getList(params?: any): Observable<GridListResponse> {
    const options = { headers: this.headers };

    if (params) {
      options['params'] = this.buildHttpParams(params);
    }

    return this.http.get<UserResponse>(this.resourcePath(), options).pipe(
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
    return this.http.post<User>(this.resourcePath(), user, {headers: this.headers });
  }

  deleteUser(userId: number): Observable<any> {
    const url = this.resourcePath(userId);
    return this.http.delete(url, { headers: this.headers });
  }

  editUser(user: User) {
    return this.http.put<User>(this.resourcePath(), user, {headers: this.headers });
  }
}
