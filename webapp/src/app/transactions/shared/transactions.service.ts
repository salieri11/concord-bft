/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { forkJoin as observableForkJoin } from 'rxjs';
import { mergeMap } from 'rxjs/operators';

import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { Transaction } from './transactions.model';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { AthenaApiService } from '../../shared/athena-api';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService extends AthenaApiService {

  constructor(
    @Inject(ATHENA_API_PREFIX) athenaApiPrefix: string,
    private httpClient: HttpClient,
    private blocksService: BlocksService
  ) {
    super(athenaApiPrefix);
  }

  get apiSubPath() {
    return 'transactions';
  }

  getTransaction(transactionHash: string) {
    return this.httpClient.get<Transaction>(this.resourcePath(transactionHash));
  }

  getRecentTransactions() {
    // Get blocks, then get individual block, then get individual transactions for all blocks.
    // This is temporary until there is an endpoint to fetch recent transactions
    return this.blocksService.getBlocks(1000).pipe(mergeMap(resp => {
      const blockObservables = resp.blocks.map((block) => this.blocksService.getBlock(block.number));
      return observableForkJoin(blockObservables).pipe(mergeMap(blocksResp => {
        const transactionsOverAllBlocks: any[] = blocksResp.map(block => block.transactions).reduce((acc, val) => acc.concat(val), []);
        return observableForkJoin(transactionsOverAllBlocks.map(transaction => this.getTransaction(transaction.hash)));
      }));
    }));
  }
}
