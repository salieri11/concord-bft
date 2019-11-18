/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';

import { Org } from './org.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

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
    if (!this.blockchainService.selectedBlockchain) { return of([]); }
    return this.http.get<Org[]>(this.orgConsortiumPath);
  }

}
