/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';

import { ATHENA_API_PREFIX } from './shared.config';

import {
  Member,
  Block,
  BlockListing,
  Transaction, BlockListingBlock
} from './remote-interfaces';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/forkJoin';
import 'rxjs/add/observable/of';

@Injectable()
export class AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) private athenaApiPrefix: string, private httpClient: HttpClient) {}

  getMembers() {
    return this.httpClient.get<Member[]>(this.apiPath('/members'));
  }

  getBlocks(count: number = 10) {
    const params = new HttpParams().set('count', count.toString());

    return this.httpClient.get<BlockListing>(this.apiPath('/blocks'), {params: params});
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

  getRecentTransactions() {
    // Get blocks, then get individual block, then build list of recent transactions from the data returned
    // This is temporary until there is an endpoint to fetch recent transactions

    return this.getBlocks(1000).flatMap(resp => {
      const blockObservables = resp.blocks.map((block) => this.getBlock(block.number));

      return Observable.forkJoin(blockObservables).flatMap(blocksResp => {
        let blockTransactions: any[] = [];

        blocksResp.forEach((block) => {
          const tempTransactions: any = (block as Block).transactions;
          tempTransactions.map(x => x.blockNumber = (block as Block).number);
          blockTransactions = blockTransactions.concat(tempTransactions);
        });

        const transactionObservables = blockTransactions.map((blockTransaction) => this.getTransaction(blockTransaction.hash));

        return Observable.forkJoin(transactionObservables).flatMap(transationsResp => {
          const transactions = transationsResp.map((transaction, index) => {
            return { blockNumber: blockTransactions[index].blockNumber, ...transaction };
          });

          return Observable.of(transactions);
        });
      });
    });
  }

  apiPath(path: string) {
    return `${this.athenaApiPrefix}${path}`;
  }
}
