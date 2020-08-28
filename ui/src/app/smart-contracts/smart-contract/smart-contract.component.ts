/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { SmartContract, SmartContractCreateResult, SmartContractVersion } from '../shared/smart-contracts.model';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { ContractFormComponent } from '../contract-form/contract-form.component';
import { TourService } from '../../shared/tour.service';
import { RouteService } from '../../shared/route.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Component({
  selector: 'concord-smart-contract',
  templateUrl: './smart-contract.component.html',
  styleUrls: ['./smart-contract.component.scss']

})
export class SmartContractComponent implements OnInit {

  @ViewChild('contractFormModal', { static: true }) contractFormModal: ContractFormComponent;

  smartContract: SmartContract;
  version: SmartContractVersion;
  versionSelected: string;

  constructor(
    private smartContractsService: SmartContractsService,
    private blockchainService: BlockchainService,
    private tourService: TourService,
    private routeService: RouteService,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

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
    const hasNewContractId = this.smartContract && this.smartContract.contract_id !== contractId;

    this.smartContractsService.getSmartContract(contractId).subscribe(
      smartContract => {
        this.smartContract = smartContract;
        if ((this.smartContract.versions.length && typeof versionId === 'undefined') || hasNewContractId) {
          // Select the latest version on load if available
          if (this.smartContract.versions[0]) {
            this.versionSelected = this.smartContract.versions[0].version;
            this.getVersionInfo();
          }
        }
      },
      e => { this.routeService.redirectToDefault(); console.log(e); }
    );
  }

  loadVersionDetails(contractId, version) {
    this.smartContractsService.getVersionDetails(contractId, version).subscribe(
      versionResponse => { this.version = versionResponse; },
      e => { this.routeService.redirectToDefault(); console.log(e); }
    );
  }

  afterUpdateContract(response: SmartContractCreateResult) {
    this.loadSmartContract(response.contract_id, response.version);
    this.versionSelected = response.version;
    this.getVersionInfo();
  }

  getVersionInfo(versionVar?: string) {
    if (versionVar) { this.versionSelected = versionVar; }
    let blockchainId = this.blockchainService.blockchainId;
    if (!blockchainId) { blockchainId = 'undefined'; }
    const path = [
      blockchainId,
      'smart-contracts',
      this.smartContract.contract_id,
      'versions',
      this.versionSelected
    ];
    const url = `/${blockchainId}/smart-contracts/`
              + this.smartContract.contract_id
              + '/versions/' + this.versionSelected;

    if (this.router.url !== url) {
      this.router.navigate(path, {replaceUrl: true});
    }
  }

  get versionIsExternal() {
    return this.version && this.version.sourcecode === '' &&
      (Object.keys(this.version.metadata).length === 0 || Object.values(this.version.metadata)[0] === 'Could not read metadata');
  }

  startTour(): void {
    this.tourService.startContractTour();
  }

  getBlockchainId(): string {
    return this.blockchainService.blockchainId;
  }
}
