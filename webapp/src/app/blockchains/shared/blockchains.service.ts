/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map } from 'rxjs/operators';
import { Observable } from 'rxjs';

import { Blockchain, BlockchainResponse } from './blockchains.model';
import { GridListResponse } from '../../grid/shared/grid.model';
import { AndesApi } from '../../shared/andes-api';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

@Injectable({
  providedIn: 'root'
})
export class BlockchainsService extends AndesApi {

  constructor(private http: HttpClient, @Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  get apiSubPath() {
    return 'clusters';
  }

  getList(params?: any): Observable<GridListResponse> {
    const options = {headers: this.headers};
    const url = this.resourcePath();

    if (params) {
      options['params'] = this.buildHttpParams(params);
    }

    return this.http.get<BlockchainResponse>(url, options).pipe(
    map(response => this.handleResponse(response)));
  }

  get(id: string): Observable<Blockchain> {
    const options = {headers: this.headers};
    const url = this.resourcePath(id);

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
    return this.http.post<Blockchain>(this.resourcePath(), blockchain, {headers: this.headers});
  }

  delete(id: number): Observable<any> {
    const url = this.resourcePath(id);
    return this.http.delete(url, {headers: this.headers});
  }
}
