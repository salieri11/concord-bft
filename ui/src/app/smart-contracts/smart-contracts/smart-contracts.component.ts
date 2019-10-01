/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { ConnectWithTruffleComponent } from '../connect-with-truffle/connect-with-truffle.component';
import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { Personas } from '../../shared/persona.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Component({
  selector: 'concord-smart-contracts',
  templateUrl: './smart-contracts.component.html',
  styleUrls: ['./smart-contracts.component.scss']
})
export class SmartContractsComponent implements OnInit {
  @ViewChild('contractFormModal', { static: true }) contractFormModal: ContractFormComponent;
  @ViewChild('truffleModal', { static: true }) truffleModal: ConnectWithTruffleComponent;

  smartContracts = [];
  personas = Personas;

  constructor (
    private smartContractsService: SmartContractsService,
    private router: Router,
    private blockchainService: BlockchainService,
  ) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  contractCreated(contract) {
    const blockchainId = this.blockchainService.blockchainId;
    const path = [
      `/${blockchainId}`,
      'smart-contracts',
      contract.contract_id,
      'versions',
      contract.version
    ];

    this.router.navigate(path);
  }

  loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }

  connectWithTruffle(): void {
    this.truffleModal.openModal();
  }
}
