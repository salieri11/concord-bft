/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { SmartContract, SmartContractVersion } from './smart-contracts.model';
import { AthenaApiService } from '../../shared/athena-api.service';

const CONTRACTS_PATH = '/contracts/';

@Injectable({
  providedIn: 'root'
})
export class SmartContractsService extends AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) athenaApiPrefix: string, private httpClient: HttpClient) {
    super(athenaApiPrefix);
  }

  getSmartContracts() {
    return this.httpClient.get<SmartContract[]>(this.apiPath(CONTRACTS_PATH));
  }

  getSmartContract(contractId: string) {
    return this.httpClient.get<SmartContract>(this.apiPath(`${CONTRACTS_PATH}${contractId}`));
  }

  getVersionDetails(contractId: string, version: string) {
    return this.httpClient.get<SmartContractVersion>(this.apiPath(`${CONTRACTS_PATH}${contractId}/versions/${version}`));
  }

  postContract(contract) {
    return this.httpClient.post<any>(this.apiPath(CONTRACTS_PATH), contract);
  }

}
