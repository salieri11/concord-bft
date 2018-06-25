/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { map } from 'rxjs/operators';
import { Observable } from 'rxjs';

import { Blockchain, BlockchainResponse } from './blockchains.model';
import { GridListResponse } from '../../grid/shared/grid.model';

@Injectable()
export class BlockchainsService {
  blockchainsUrl = '/api/clusters';
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type':  'application/json',
    // 'Authorization': 'my-auth-token'
  });

  constructor(private http: HttpClient) {}

  getList(params?: any): Observable<GridListResponse> {
    const options = {headers: this.headers};
    const url = this.blockchainsUrl;

    if (params) {
      let httpParams = new HttpParams();

      for (const prop in params) {
        if (params[prop]) {
          httpParams = httpParams.set(prop, params[prop]);
        }
      }

      options['params'] = httpParams;
    }

    return this.http.get<BlockchainResponse>(url, options).pipe(
    map(response => this.handleResponse(response)));
  }

  get(id: string): Observable<Blockchain> {
    const options = {headers: this.headers};
    const url = `${this.blockchainsUrl}/${id}`;

    return this.http.get<Blockchain>(url, options);
  }

  private handleResponse(response: BlockchainResponse): GridListResponse {

    return {
      objects: response._embedded ? response._embedded.clusters : [],
      meta: {
        size: response.page ? response.page.size : 0,
        total: response.page ? response.page.totalElements : 0,
        totalPages: response.page ? response.page.totalPages : 0
      }
    };
  }

  create(blockchain: any): Observable<any> {
    return this.http.post<Blockchain>(this.blockchainsUrl, blockchain, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    const url = `${this.blockchainsUrl}/${id}`;
    return this.http.delete(url, {headers: this.headers});
  }
}
