/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { forkJoin as observableForkJoin } from 'rxjs';
import { mergeMap } from 'rxjs/operators';

import { CONCORD_API_PREFIX } from '../../shared/shared.config';
import { Transaction, TransactionListing } from './transactions.model';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { concordApiService } from '../../shared/concord-api';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService extends concordApiService {

  constructor(
    @Inject(CONCORD_API_PREFIX) concordApiPrefix: string,
    private httpClient: HttpClient,
    private blocksService: BlocksService
  ) {
    super(concordApiPrefix);
  }

  get apiSubPath() {
    return 'transactions';
  }

  getTransaction(transactionHash: string) {
    return this.httpClient.get<Transaction>(this.resourcePath(transactionHash));
  }

  getTransactions(count: number) {
    const params = new HttpParams().set('count', count.toString());

    return this.httpClient.get<TransactionListing>(this.resourcePath(), {params: params});
  }

  getRecentTransactions(count: number = 1000) {
    // Get blocks, then get individual block, then get individual transactions for all blocks.
    // This is temporary until there is an endpoint to fetch recent transactions
    return this.blocksService.getBlocks(count).pipe(mergeMap(resp => {
      const blockObservables = resp.blocks.map((block) => this.blocksService.getBlock(block.number));
      return observableForkJoin(blockObservables).pipe(mergeMap(blocksResp => {
        const transactionsOverAllBlocks: any[] = blocksResp.map(block => block.transactions).reduce((acc, val) => acc.concat(val), []);
        return observableForkJoin(transactionsOverAllBlocks.map(transaction => this.getTransaction(transaction.hash)));
      }));
    }));
  }
}
