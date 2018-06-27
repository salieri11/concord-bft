/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { of as observableOf, forkJoin as observableForkJoin } from 'rxjs';
import { mergeMap } from 'rxjs/operators';

import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { Block, Transaction } from '../../shared/remote-interfaces';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { AthenaApiService } from '../../shared/athena-api.service';

const TRANSACTIONS_PATH = '/transactions/';

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


  getTransaction(transactionHash: string) {
    return this.httpClient.get<Transaction>(this.apiPath(`${TRANSACTIONS_PATH}${transactionHash}`));
  }

  getRecentTransactions() {
    // Get blocks, then get individual block, then build list of recent transactions from the data returned
    // This is temporary until there is an endpoint to fetch recent transactions

    return this.blocksService.getBlocks(1000).pipe(mergeMap(resp => {
      const blockObservables = resp.blocks.map((block) => this.blocksService.getBlock(block.number));

      return observableForkJoin(blockObservables).pipe(mergeMap(blocksResp => {
        let blockTransactions: any[] = [];

        blocksResp.forEach((block) => {
          const tempTransactions: any = (block as Block).transactions;
          tempTransactions.map(x => x.blockNumber = (block as Block).number);
          blockTransactions = blockTransactions.concat(tempTransactions);
        });

        const transactionObservables = blockTransactions.map((blockTransaction) => this.getTransaction(blockTransaction.hash));

        return observableForkJoin(transactionObservables).pipe(mergeMap(transationsResp => {
          const transactions = transationsResp.map((transaction, index) => {
            return { blockNumber: blockTransactions[index].blockNumber, ...transaction };
          });

          return observableOf(transactions);
        }));
      }));
    }));
  }
}
