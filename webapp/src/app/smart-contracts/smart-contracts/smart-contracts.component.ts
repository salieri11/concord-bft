/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { AthenaApiService } from '../../shared/athena-api.service';
import { ContractFormComponent } from '../contract-form/contract-form.component';

@Component({
  selector: 'athena-smart-contracts',
  templateUrl: './smart-contracts.component.html',
  styleUrls: ['./smart-contracts.component.scss']
})
export class SmartContractsComponent implements OnInit {
  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;

  smartContracts = [];

  constructor( private athenaApiService: AthenaApiService ) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  loadSmartContracts() {
    this.athenaApiService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }
}
