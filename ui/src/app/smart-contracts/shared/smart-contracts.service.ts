/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { SmartContract, SmartContractVersion, SmartContractVersionHeader } from './smart-contracts.model';
import { tap } from 'rxjs/operators';
import { of } from 'rxjs';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { Apis } from '../../shared/urls.model';

export interface TargetContractData {
  contract_id: string;
  blockchain_id: string;
  header: SmartContractVersionHeader;
  data?: SmartContractVersion;
}
interface SmartContractRegistry {
  [contractId: string]: SmartContract;
}

@Injectable({
  providedIn: 'root'
})
export class SmartContractsService {

  private contractVersionByAddress = new Map<string, TargetContractData>();
  private contractsCached: SmartContractRegistry = {};

  constructor(
    private httpClient: HttpClient,
    private blockchainService: BlockchainService
  ) { }

  getCompilerVersion() {
    return this.httpClient.get<any>(Apis.contractCompileVersions);
  }

  getSmartContracts() {
    return this.httpClient.get<SmartContract[]>(
      Apis.contracts(this.blockchainService.blockchainId));
  }

  getSmartContract(contractId: string) {
    return this.httpClient.get<SmartContract>(
      Apis.contract(this.blockchainService.blockchainId, contractId));
  }

  getVersionDetails(contractId: string, version: string) {
    return this.httpClient.get<SmartContractVersion>(
      Apis.contractVersion(this.blockchainService.blockchainId, contractId, version));
  }

  updateExistingVersion(contractId: string, request) {
    return this.httpClient.put<SmartContractVersion>(
      Apis.contract(this.blockchainService.blockchainId, contractId), request);
  }

  postContract(contract) {
    return this.httpClient.post<any>(
      Apis.contracts(this.blockchainService.blockchainId), contract);
  }

  postSourceCode(request) {
    return this.httpClient.post<any>(Apis.contractCompile, request);
  }

  async getContractByAddress(address: string): Promise<TargetContractData> {
      try {
        const cached = this.contractVersionByAddress.get(address);
        if (cached) { return cached; }

        const targetContract = await this.getVersionInfoByAddress(address);
        if (!targetContract) { return null; }

        const fullData = await this.getVersionDetails(targetContract.contract_id,
                                    targetContract.header.version).toPromise();
        if (!fullData) { return null; }

        targetContract.data = fullData;
        this.contractVersionByAddress.set(address, targetContract);
        return targetContract;
      } catch (e) { console.log(e); return null; }
  }

  private async getVersionInfoByAddress(address: string): Promise<TargetContractData> {
    let versionInfo = this.getContractDataByAddress(address);
    if (versionInfo) { return versionInfo; }

    try {
      const allFetches = [];
      const contracts = await this.getSmartContracts().toPromise();
      for (const contract of contracts) {
        const fetcher = this.getSmartContract(contract.contract_id).pipe(tap(data => {
          this.contractsCached[data.contract_id] = data;
        })).toPromise();
        allFetches.push(fetcher);
      }
      await Promise.all(allFetches);
      versionInfo = this.getContractDataByAddress(address);
    } catch (e) { console.log(e); return null; }

    return versionInfo;
  }

  private getContractDataByAddress(address: string): TargetContractData {
    for (const contractId of Object.keys(this.contractsCached)) {
      const contract = this.contractsCached[contractId];
      if (!contract || !contract.versions || contract.versions.length === 0) { continue; }
      for (const versionHeader of contract.versions) {
        if (versionHeader.address === address) {
          return {
            contract_id: contract.contract_id,
            blockchain_id: this.blockchainService.blockchainId,
            header: versionHeader
          };
        }
      }
    }
    return null;
  }

}


export class MockSmartContractsService {

  contractToolsApi: string = 'api/concord/contracts/';
  mockSmartContract = { contract_id: 1, versions: [{version: '1'}] };

  get apiSubPath() { return 'contracts'; }

  getCompilerVersion() { return of([]); }
  getSmartContracts() { return of([this.mockSmartContract]); }
  getSmartContract() { return of(this.mockSmartContract); }
  getVersionDetails() { return of({}); }
  updateExistingVersion() { return of({}); }
  postContract() { return of({}); }
  postSourceCode() { return of({}); }

  async getContractByAddress() { return null; }

}
