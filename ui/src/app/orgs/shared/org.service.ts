/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { Apis } from '../../shared/urls.model';

import { Org } from './org.model';

@Injectable({
  providedIn: 'root'
})
export class OrgService {
  get orgConsortiumPath() {
    return `/api/consortiums/${this.blockchainService.selectedBlockchain.consortium_id}/organizations`;
  }

  constructor(
    private http: HttpClient,
    private blockchainService: BlockchainService
    ) {
  }

  getList(): Observable<Org[]> {
    return this.http.get<Org[]>(this.orgConsortiumPath);
  }

  getDetail(id: string): Observable<Org> {
    return this.http.get<Org>(`${Apis.organizations}/${id}`);
  }

}
