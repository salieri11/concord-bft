import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';

import { Observable } from 'rxjs/Observable';
import 'rxjs/add/operator/map';
import { ErrorObservable } from 'rxjs/observable/ErrorObservable';
import { Org, OrgResponse } from './org-management.model';
import { GridListResponse } from '../../grid/shared/grid.model';


@Injectable()
export class OrgManagementService {
  orgUrl: string = '/api/organizations';
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type':  'application/json',
    // 'Authorization': 'my-auth-token'
  });

  constructor(private http: HttpClient) {

  }

  getList(params: any, changeUrl?: string): Observable<GridListResponse> {
    let options = {headers: this.headers};
    let url = this.orgUrl;

    if (changeUrl) {
      url = changeUrl;
    }

    if (params) {
      let httpParams = new HttpParams();

      for (let prop in params) {
        httpParams = httpParams.set(prop, params[prop]);
      }

      options['params'] = httpParams;
    }

    return this.http.get<OrgResponse>(url, options)
      .map(response => this.handleResponse(response));
  }

  getUsableOrgs(): Observable<GridListResponse> {
    const url = '/api/organizations/search/usablePeerOrgs'
    let options = {headers: this.headers};

    return this.http.get<any>(url, options)
    .map(response => this.handleResponse(response));
  }

  getOrdererOrgs(): Observable<GridListResponse> {
    let options = {headers: this.headers};
    const url = 'api/organizations/search/usableOrdererOrgs'

    return this.http.get<any>(url, options)
    .map(response => this.handleResponse(response));
  }

  private handleResponse(response: OrgResponse): GridListResponse {

    return {
      objects: response._embedded ? response._embedded.organizations : [],
      meta: {
        size: response.page ? response.page.size : 0,
        total: response.page ? response.page.totalElements : 0,
        totalPages: response.page ? response.page.totalPages: 0
      }
    }
  }

  create(org: Org): Observable<any> {
    return this.http.post<Org>(this.orgUrl, org, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    let url = `${this.orgUrl}/${id}`
    return this.http.delete(url, {headers: this.headers});
  }

  import(body): Observable<any> {
    let url = `${this.orgUrl}/import`;

    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'Application/Octet-Stream'
        // 'Authorization': 'my-auth-token'
      })
    };

    return this.http.post(url, body, {headers: this.headers});
  }

  getFakeData(params: any): Observable<Array<Org>> {
    let d = new Date(),
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
