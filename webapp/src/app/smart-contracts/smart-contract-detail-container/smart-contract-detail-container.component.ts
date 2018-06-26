/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { AthenaApiService } from '../../shared/athena-api.service';
import { SmartContract, SmartContractVersion } from '../../shared/remote-interfaces';

@Component({
  selector: 'app-smart-contract-detail-container',
  templateUrl: './smart-contract-detail-container.component.html',
  styleUrls: ['./smart-contract-detail-container.component.scss']

})
export class SmartContractDetailContainerComponent implements OnInit {

  smartContract: SmartContract;
  version: SmartContractVersion;
  versionSelected;

  constructor(private athenaApiService: AthenaApiService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      if (!this.smartContract || this.smartContract.contract_id !== params.contractId) {
        debugger;
        this.loadSmartContract(params.contractId);
      }
      if (params.version) {
        this.versionSelected = params.version;
        this.loadVersionDetails(params.contractId, params.version);
      }
    });
  }

  loadSmartContract(contractId) {
    this.athenaApiService.getSmartContract(contractId).subscribe(smartContract => this.smartContract = smartContract);
    debugger;
  }

  loadVersionDetails(contractId, version) {
    this.athenaApiService.getVersionDetails(contractId, version).subscribe(versionResponse => this.version = versionResponse);
    debugger;
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
