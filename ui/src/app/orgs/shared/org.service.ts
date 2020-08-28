/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { Apis } from '../../shared/urls.model';

import { Org, mockOrgDefault, mockOrgs } from './org.model';

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

export class MockOrgService {
  orgs = mockOrgs;
  currentOrg = mockOrgDefault;
  orgConsortiumPath = '/api/consortiums/aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa/organizations';

  getList() { return of(this.orgs); }
  getDetail() { return of({}); }

  // Unit Test Only Functions
  provideDefault() { this.orgs = mockOrgs; this.currentOrg = mockOrgDefault; }
}
