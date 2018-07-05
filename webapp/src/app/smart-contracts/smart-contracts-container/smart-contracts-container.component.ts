/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-smart-contracts-container',
  templateUrl: './smart-contracts-container.component.html',
  styleUrls: ['./smart-contracts-container.component.scss']
})
export class SmartContractsContainerComponent implements OnInit {
  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;

  smartContracts = [];
  personas = Personas;

  constructor( private smartContractsService: SmartContractsService ) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }
}
