/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

import { Consortium, ConsortiumResponse } from './consortium.model';
import { GridListResponse } from '../../grid/shared/grid.model';
import { AndesApi } from '../../shared/andes-api';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

@Injectable({
  providedIn: 'root'
})
export class ConsortiumService extends AndesApi {

  constructor(private http: HttpClient, @Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  get apiSubPath() {
    return 'consortium';
  }

  getList(params?: any): Observable<GridListResponse> {
    const options = { headers: this.headers };

    if (params) {
      options['params'] = this.buildHttpParams(params);
    }

    return this.http.get<ConsortiumResponse>(this.resourcePath(), options).pipe(
      map(response => this.handleResponse(response)));
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
    return this.http.post<Consortium>(this.resourcePath(), org, { headers: this.headers });
  }

  delete(id: number): Observable<any> {
    const url = this.resourcePath(id);
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
