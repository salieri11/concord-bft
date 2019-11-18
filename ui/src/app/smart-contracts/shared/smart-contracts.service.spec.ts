/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { MockSharedModule } from '../../shared/shared.module';

import { SmartContractsService } from './smart-contracts.service';

describe('SmartContractsService', () => {
  let service: SmartContractsService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [MockSharedModule],
      providers: [
        SmartContractsService,
        HttpClient,
        HttpHandler
      ]
    });

    service = TestBed.get(SmartContractsService);
  });

  it('should be created', inject([SmartContractsService], (contractService: SmartContractsService) => {
    expect(contractService).toBeTruthy();
  }));

  it('should fetch all smart contracts', () => {
      const httpSpy = spyOn((service as any).httpClient, 'get');
      service.getSmartContracts();
      expect(httpSpy).toHaveBeenCalledWith(`/api/blockchains/undefined/concord/contracts`);
    }
  );

  it('should fetch smart contract with given id', () => {
      const httpSpy = spyOn((service as any).httpClient, 'get');
      service.getSmartContract('contractId');
      expect(httpSpy).toHaveBeenCalledWith(`/api/blockchains/undefined/concord/contracts/contractId`);
  });

  it('should fetch version details for given versionId and contractId', () => {
    const httpSpy = spyOn((service as any).httpClient, 'get');
    service.getVersionDetails('contractId', 'version');
    expect(httpSpy).toHaveBeenCalledWith(`/api/blockchains/undefined/concord/contracts/contractId/versions/version`);
  });

  it('should post a smart contract with given payload', () => {
    const httpSpy = spyOn((service as any).httpClient, 'post');
    const contract = {
      id: 1,
      from: '0x262C0D7AB5FFD4EDE2199F6EA793F819E1ABB019',
      contract_id: '1',
      version: '1',
      sourcecode: 'sourceCode'
    };

    service.postContract(contract);
    expect(httpSpy).toHaveBeenCalledWith(`/api/blockchains/undefined/concord/contracts`, contract);
  });

  it('should put a smart contract with given payload', () => {
    const httpSpy = spyOn((service as any).httpClient, 'put');
    const contract = {
      from: '0x262C0D7AB5FFD4EDE2199F6EA793F819E1ABB019',
      contract_id: '1',
      version: '1',
      sourcecode: 'sourceCode',
      contractName: 'contract',
      constructorParams: '',
      compilerVersion: 'v0.5.4+commit.9549d8ff'
    };

    service.updateExistingVersion('contractId', contract);
    expect(httpSpy).toHaveBeenCalledWith(`/api/blockchains/undefined/concord/contracts/contractId`, contract);
  });

  it('should post smart contract source code', () => {
    const httpSpy = spyOn((service as any).httpClient, 'post');
    const payload = {
      sourcecode: 'source',
      compilerVersion: 'v0.5.4+commit.9549d8ff'
    };

    service.postSourceCode(payload);
    expect(httpSpy).toHaveBeenCalledWith('/api/concord/contracts/compile', payload);
  });

});
