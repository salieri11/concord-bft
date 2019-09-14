/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';

import { CONCORD_API_PREFIX } from '../../shared/shared.config';
import { Block, BlockListing } from './blocks.model';
import { ConcordApiService } from '../../shared/concord-api';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Injectable({
  providedIn: 'root'
})
export class BlocksService extends ConcordApiService {

  constructor(
    @Inject(CONCORD_API_PREFIX) concordApiPrefix: string,
    private httpClient: HttpClient,
    // @ts-ignore: no unused locals
    private blockchainService: BlockchainService
  ) {
    super(concordApiPrefix);
  }

  get apiSubPath() {
    return 'blocks';
  }

  getBlocks(count: number = 10) {
    const params = new HttpParams().set('count', count.toString());

    return this.httpClient.get<BlockListing>(this.resourcePath(), {params: params});
  }

  getBlocksByUrl(url: string) {
    return this.httpClient.get<BlockListing>(url);
  }

  getBlock(blockNumber) {
    return this.httpClient.get<Block>(this.resourcePath(blockNumber));
  }
}
