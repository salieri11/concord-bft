/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { ATHENA_API_PREFIX } from './shared.config';

import {
  Member,
  Block,
  BlockListing,
  Transaction
} from './remote-interfaces';

@Injectable()
export class AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) private athenaApiPrefix: string, private httpClient: HttpClient) {}

  getMembers() {
    return this.httpClient.get<Member[]>(this.apiPath('/members'));
  }

  getBlocks() {
    return this.httpClient.get<BlockListing>(this.apiPath('/blocks'));
  }

  getBlocksByUrl(url: string) {
    return this.httpClient.get<BlockListing>(url);
  }

  getBlock(blockNumber) {
    return this.httpClient.get<Block>(this.apiPath(`/blocks/${blockNumber}`));
  }

  getTransaction(transactionHash: string) {
    return this.httpClient.get<Transaction>(this.apiPath(`/transactions/${transactionHash}`));
  }

  apiPath(path: string) {
    return `${this.athenaApiPrefix}${path}`;
  }
}
