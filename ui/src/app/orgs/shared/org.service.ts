/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { OrgResponse } from './org.model';
import { GridListResponse } from '../../grid/shared/grid.model';
import { BaseApi } from '../../base-api';
import { BlockchainService } from '../../shared/blockchain.service';

@Injectable({
  providedIn: 'root'
})
export class OrgService extends BaseApi {

  constructor(
    private http: HttpClient,
    // @ts-ignore
    private blockchainService: BlockchainService
    ) {
    super();
  }

  get apiPath() {
    return 'api';
  }

  get apiSubPath() {
    return 'organizations';
  }

  getList(): Observable<GridListResponse> {
    return this.http.get<OrgResponse>(this.resourcePath()).pipe(
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

}
