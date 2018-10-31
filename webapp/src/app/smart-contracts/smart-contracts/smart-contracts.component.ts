/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';

import { ConnectWithTruffleComponent } from '../connect-with-truffle/connect-with-truffle.component';
import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-smart-contracts',
  templateUrl: './smart-contracts.component.html',
  styleUrls: ['./smart-contracts.component.scss']
})
export class SmartContractsComponent implements OnInit {
  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;
  @ViewChild('truffleModal') truffleModal: ConnectWithTruffleComponent;

  smartContracts = [];
  personas = Personas;

  constructor (
    private smartContractsService: SmartContractsService,
    private router: Router
  ) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  contractCreated(contract) {
    const path = [
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
