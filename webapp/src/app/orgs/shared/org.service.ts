/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { map } from 'rxjs/operators';
import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { Org, OrgResponse } from './org.model';
import { GridListResponse } from '../../grid/shared/grid.model';
import { AndesApi } from '../../shared/andes-api';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

@Injectable({
  providedIn: 'root'
})
export class OrService extends AndesApi {

  constructor(private http: HttpClient, @Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  get apiSubPath() {
    return 'organizations';
  }

  getList(params: any, changeUrl?: string): Observable<GridListResponse> {
    const options = {headers: this.headers};
    let url = this.resourcePath();

    if (changeUrl) {
      url = changeUrl;
    }

    if (params) {
      options['params'] = this.buildHttpParams(params);
    }

    return this.http.get<OrgResponse>(url, options).pipe(
      map(response => this.handleResponse(response)));
  }

  getUsableOrgs(): Observable<GridListResponse> {
    const url = this.resourcePath('search/usablePeerOrgs');
    const options = {headers: this.headers};

    return this.http.get<any>(url, options).pipe(
    map(response => this.handleResponse(response)));
  }

  getOrdererOrgs(): Observable<GridListResponse> {
    const options = {headers: this.headers};
    const url = this.resourcePath('search/usableOrdererOrgs');

    return this.http.get<any>(url, options).pipe(
    map(response => this.handleResponse(response)));
  }

  private handleResponse(response: OrgResponse): GridListResponse {

    return {
      objects: response._embedded ? response._embedded.organizations : [],
      meta: {
        size: response.page ? response.page.size : 0,
        total: response.page ? response.page.totalElements : 0,
        totalPages: response.page ? response.page.totalPages : 0
      }
    };
  }

  create(org: Org): Observable<any> {
    return this.http.post<Org>(this.resourcePath(), org, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    const url = this.resourcePath(id);
    return this.http.delete(url, {headers: this.headers});
  }

  import(body): Observable<any> {
    const url = this.resourcePath('import');

    return this.http.post(url, body, {headers: this.headers});
  }

  getFakeData(): Observable<Array<Org>> {
    const d = new Date(),
      data = [{
        id: 1,
        name: 'Org1',
        domain: 'org1',
        peerNumber: 1234,
        type: 'peer',
        created: d.setDate(d.getDate() - 5),
      }, {
        id: 1,
        name: 'Org2',
        domain: 'org2',
        peerNumber: 1234,
        type: 'peer1',
        created: d.setDate(d.getDate() - 10),
      }, {
        id: 1,
        name: 'Org3',
        domain: 'org3',
        peerNumber: 1234,
        type: 'peer2',
        created: d.setDate(d.getDate() - 15),
      }];

    return new Observable(observer => {
      observer.next(data);
      observer.complete();
    });
  }

}
