/*
 * Copyright 2018 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';

import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';

import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/map';
import { Consortium, ConsortiumResponse } from './consortium.model';
import { GridListResponse } from '../../grid/shared/grid.model';

@Injectable()
export class ConsortiumService {
  consortiumUrl = '/api/consortium';
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type': 'application/json',
    // 'Authorization': 'my-auth-token'
  });

  constructor(private http: HttpClient) { }

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

    return this.http.get<ConsortiumResponse>(this.consortiumUrl, options)
      .map(response => this.handleResponse(response));
  }

  private handleResponse(response: ConsortiumResponse): GridListResponse {

    return {
      objects: response._embedded.organizations,
      meta: {
        size: response.page.size,
        total: response.page.totalElements,
        totalPages: response.page.totalPages
      }
    };
  }

  create(org: Consortium): Observable<any> {
    return this.http.post<Consortium>(this.consortiumUrl, org, { headers: this.headers });
  }

  delete(id: number): Observable<any> {
    const url = `${this.consortiumUrl}${id}/`;
    return this.http.delete(url, { headers: this.headers });
  }

  getFakeData(): Observable<GridListResponse> {
    const d = new Date(),
      data = {
        objects: [{
          id: 1,
          name: 'Consortium A',
          members: ['Org1', 'Org2', 'Org3'],
          createdOn: d.setDate(d.getDate() - 5),
        }, {
          id: 1,
          name: 'Consortium B',
          members: ['Org1', 'Org2', 'Org3'],
          createdOn: d.setDate(d.getDate() - 10),
        }, {
          id: 1,
          name: 'Consortium C',
          members: ['Org1', 'Org2', 'Org3'],
          createdOn: d.setDate(d.getDate() - 15),
        }],
        meta: {
          size: 10,
          total: 10,
          totalPages: 10
        }
      };

    return new Observable(observer => {
      observer.next(data);
      observer.complete();
    });
  }

}
