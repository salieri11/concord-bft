/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';

import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { Block, BlockListing } from './blocks.model';
import { AthenaApiService } from '../../shared/athena-api';

@Injectable({
  providedIn: 'root'
})
export class BlocksService extends AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) athenaApiPrefix: string, private httpClient: HttpClient) {
    super(athenaApiPrefix);
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
