/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { SmartContract, SmartContractCreateResult, SmartContractVersion } from '../shared/smart-contracts.model';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { ContractFormComponent } from '../contract-form/contract-form.component';

@Component({
  selector: 'athena-smart-contract',
  templateUrl: './smart-contract.component.html',
  styleUrls: ['./smart-contract.component.scss']

})
export class SmartContractComponent implements OnInit {

  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;
  smartContract: SmartContract;
  version: SmartContractVersion;
  versionSelected;

  constructor(private smartContractsService: SmartContractsService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit() {
    this.route.params.subscribe(params => {
      if (!this.smartContract || this.smartContract.contract_id !== params.contractId) {
        this.loadSmartContract(params.contractId, params.version);
      }
      if (params.version) {
        this.versionSelected = params.version;
        this.loadVersionDetails(params.contractId, params.version);
      }
    });
  }

  loadSmartContract(contractId, versionId?) {
    this.smartContractsService.getSmartContract(contractId).subscribe((smartContract) => {
      this.smartContract = smartContract;

      if (this.smartContract.versions.length && typeof versionId === 'undefined') {
        // Select the latest version on load if available
        this.versionSelected = this.smartContract.versions[0].version;
        this.getVersionInfo();
      }
    });
  }

  loadVersionDetails(contractId, version) {
    this.smartContractsService.getVersionDetails(contractId, version).subscribe(versionResponse => this.version = versionResponse);
  }

  afterUpdateContract(response: SmartContractCreateResult) {
    this.loadSmartContract(response.contract_id, response.version);
    this.versionSelected = response.version;
    this.getVersionInfo();
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
