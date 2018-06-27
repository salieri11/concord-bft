/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { SmartContract, SmartContractVersion } from '../shared/smart-contracts.model';
import { SmartContractsService } from '../shared/smart-contracts.service';

@Component({
  selector: 'athena-smart-contract-detail-container',
  templateUrl: './smart-contract-detail-container.component.html',
  styleUrls: ['./smart-contract-detail-container.component.scss']

})
export class SmartContractDetailContainerComponent implements OnInit {

  smartContract: SmartContract;
  version: SmartContractVersion;
  versionSelected;

  constructor(private smartContractsService: SmartContractsService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      if (!this.smartContract || this.smartContract.contract_id !== params.contractId) {
        this.loadSmartContract(params.contractId);
      }
      if (params.version) {
        this.versionSelected = params.version;
        this.loadVersionDetails(params.contractId, params.version);
      }
    });
  }

  loadSmartContract(contractId) {
    this.smartContractsService.getSmartContract(contractId).subscribe(smartContract => this.smartContract = smartContract);
  }

  loadVersionDetails(contractId, version) {
    this.smartContractsService.getVersionDetails(contractId, version).subscribe(versionResponse => this.version = versionResponse);
  }

  getVersionInfo() {
    const path = [
      'smart-contracts',
      this.smartContract.contract_id,
      'versions',
      this.versionSelected
    ];
    this.router.navigate(path);
  }
}
