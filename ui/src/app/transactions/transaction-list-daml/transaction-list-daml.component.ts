/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit, OnChanges, SimpleChanges } from '@angular/core';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { HttpClient } from '@angular/common/http';
import { AbiFunctionDefinition } from '../../smart-contracts/shared/smart-contracts.model';
import { damlGetPropertyName, damlGetPropertyData, damlFlattenStruct } from '../../shared/daml.utils';
import { tap } from 'rxjs/operators';

@Component({
  selector: 'concord-transaction-list-daml',
  templateUrl: './transaction-list-daml.component.html',
  styleUrls: ['./transaction-list-daml.component.scss']
})
export class TransactionListDamlComponent implements OnInit, OnChanges {
  transactions: any[] = [];
  @Input() templateDef: AbiFunctionDefinition;
  @Input() structMap: any;
  @Input() moduleName: string;
  @Input() entityName: string;
  @Input() pagination: number = 20;
  blockchainId: string;
  templateProperties: any[] = [];
  transactionsLoaded: boolean = false;
  transactionsFetchError: string;

  constructor(
    private http: HttpClient,
    private blockchainService: BlockchainService,
  ) {
    if (!this.pagination) { this.pagination = 20; }
  }

  ngOnInit() {
    if (!this.pagination) { this.pagination = 20; }
    this.blockchainId = this.blockchainService.blockchainId;
    if (this.moduleName && this.entityName) { this.loadRecentTransactions(); }
  }

  loadRecentTransactions() {
    this.transactionsLoaded = false;
    // TODO: (from MR 994) move this to Smart Contract Service
    this.http.post<any[]>(`/api/blockchains/${this.blockchainId}/concord/contracts/daml`, {
      moduleName: this.moduleName,
      entityName: this.entityName,
    }).pipe(
      tap((transactions) => {
        if (!Array.isArray(transactions)) {
          this.transactionsFetchError = transactions['error'];
          this.transactions = [];
        } else {
          this.transactionsFetchError = null;
          this.transactions = transactions;
        }
        this.transactionsLoaded = true;
        return transactions;
      })
    ).subscribe();
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.templateDef && changes.templateDef.currentValue) {
      this.templateProperties = damlFlattenStruct(this.templateDef.inputs, this.structMap);
      this.loadRecentTransactions();
    }
  }

  getPropertyName(inputName: string) { return damlGetPropertyName(inputName); }

  getPropertyData(transaction, inputName) { return damlGetPropertyData(transaction, inputName); }

}
