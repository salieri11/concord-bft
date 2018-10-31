/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { SmartContract, SmartContractVersion } from './smart-contracts.model';
import { AthenaApiService } from '../../shared/athena-api';

@Injectable({
  providedIn: 'root'
})
export class SmartContractsService extends AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) athenaApiPrefix: string, private httpClient: HttpClient) {
    super(athenaApiPrefix);
  }

  get apiSubPath() {
    return 'contracts';
  }

  getSmartContracts() {
    return this.httpClient.get<SmartContract[]>(this.resourcePath());
  }

  getSmartContract(contractId: string) {
    return this.httpClient.get<SmartContract>(this.resourcePath(contractId));
  }

  getVersionDetails(contractId: string, version: string) {
    return this.httpClient.get<SmartContractVersion>(this.resourcePath(`${contractId}/versions/${version}`));
  }

  updateExistingVersion(contractId: string, version: string, request) {
    return this.httpClient.put<SmartContractVersion>(this.resourcePath(`${contractId}/versions/${version}`), request);
  }

  postContract(contract) {
    return this.httpClient.post<any>(this.resourcePath(), contract);
  }

  postSourceCode(request) {
    return this.httpClient.post<any>(this.resourcePath('compile'), request);
  }

}
