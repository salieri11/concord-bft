/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { Node } from './nodes.model';
import { CONCORD_API_PREFIX } from '../../shared/shared.config';
import { ConcordApiService } from '../../shared/concord-api';
import { BlockchainService } from '../../shared/blockchain.service';

@Injectable({
  providedIn: 'root'
})
export class NodesService extends ConcordApiService {

  constructor(
    @Inject(CONCORD_API_PREFIX) concordApiPrefix: string,
    private httpClient: HttpClient,
    // @ts-ignore: no unused locals
    private blockchainService: BlockchainService
  ) {
    super(concordApiPrefix);
  }

  get apiSubPath() {
    return 'members';
  }

  getNodes(): Observable<Node[]> {
    return this.httpClient.get<Node[]>(this.resourcePath());
  }
}
