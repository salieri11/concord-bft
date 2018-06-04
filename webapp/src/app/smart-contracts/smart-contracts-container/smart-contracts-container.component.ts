/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { AthenaApiService } from '../../shared/athena-api.service';
import { CreateContractModalComponent } from '../create-contract-modal/create-contract-modal.component';

@Component({
  selector: 'app-smart-contracts-container',
  templateUrl: './smart-contracts-container.component.html',
  styleUrls: ['./smart-contracts-container.component.scss']
})
export class SmartContractsContainerComponent implements OnInit {
  @ViewChild('createContractModal') createContractModal: CreateContractModalComponent;

  smartContracts = [];

  constructor( private athenaApiService: AthenaApiService ) { }


  ngOnInit() {
    this.loadSmartContracts();
  }

  loadSmartContracts() {
    this.athenaApiService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts.result);
  }
}
