/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';

import { Block, BlockListing } from './blocks.model';
import { of } from 'rxjs';
import { Apis } from '../../shared/urls.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Injectable({
  providedIn: 'root'
})
export class BlocksService {

  constructor(
    private httpClient: HttpClient,
    private blockchainService: BlockchainService
  ) { }

  getBlocks(count: number = 10) {
    const params = new HttpParams().set('count', count.toString());
    return this.httpClient.get<BlockListing>(
      Apis.blocks(this.blockchainService.blockchainId), {params: params});
  }

  getBlocksByUrl(url: string) {
    return this.httpClient.get<BlockListing>(url);
  }

  getBlock(blockNumberOrHash) {
    return this.httpClient.get<Block>(Apis.block(
      this.blockchainService.blockchainId, blockNumberOrHash));
  }
}


export class MockBlocksService {

  mockBlock = {
    hash: '0x0000',
    nonce: '0x0000',
    parentHash: '0x0000',
    number: 1,
    size: 1,
    transactions: [{hash: '0x0000', url: '/' }]
  };

  getBlocks(count: number = 10) { return of({next: '', blocks: [], count: count}); }

  getBlocksByUrl(url: string) { return of({next: '', blocks: [], url: url}); }

  getBlock() { return of(this.mockBlock); }
}
