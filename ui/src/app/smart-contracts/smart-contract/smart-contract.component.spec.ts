/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { of as observableOf } from 'rxjs';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';

import { SmartContractComponent } from './smart-contract.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { mainRoutes } from '../../shared/urls.model';

describe('SmartContractComponent', () => {
  let smartContractService: SmartContractsService;

  const test = testFor(SmartContractComponent).expedite({
    imports: [], provides: [],
    declarations: [SmartContractComponent],
    router: `/test/${mainRoutes.smartContracts}/2/1`,
    route: { params: { contractId: '2', version: '1' } }
  }, beforeTesting(() => {
    smartContractService = test.getService(SmartContractsService);
  }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });

  it('should load smart contract with given contractId', () => {
    const spy = spyOn(smartContractService, 'getSmartContract')
      .and.returnValue(observableOf({contract_id: 'smart contract', versions: []}));
    test.component.loadSmartContract('contractId');
    expect(spy).toHaveBeenCalled();
  });

  it('should load version details for given contractId and version', () => {
    const spy = spyOn(smartContractService, 'getVersionDetails')
      .and.returnValue(observableOf({ versionDetails: 'version details' }));
    test.component.loadVersionDetails('contractId', 'version');
    expect(spy).toHaveBeenCalled();
  });

  it('should load the first version for a contract if the version param is not supplied and the contract has versions', () => {
    const contract = {
      contract_id: 'contractId',
      versions: [{
        address: 'address',
        metadata: {},
        version: 'version',
        url: 'url'
      }]
    };

    const spy = spyOn(test.component, 'getVersionInfo');
    spyOn(test.componentProperty('smartContractsService'), 'getSmartContract').and.returnValue(observableOf(contract));

    test.component.loadSmartContract('contractId');

    expect(test.component.versionSelected).toBe(contract.versions[0].version);
    expect(spy).toHaveBeenCalled();
  });

  it('should navigate to the version details page', async () => {
    const spy = spyOn(test.router, 'navigate');
    test.component.smartContract = {
      contract_id: '1',
      owner: 'owner',
      versions: []
    };

    test.component.versionSelected = '1';
    test.component.getVersionInfo();
    let blockchainId = test.component.getBlockchainId();
    if (!blockchainId) { blockchainId = 'undefined'; }
    expect(spy).toHaveBeenCalledWith([
      blockchainId,
      'smart-contracts',
      '1',
      'versions',
      '1'], Object({ replaceUrl: true }));
  });


  it('should load smartContract with contractId in the params', () => {
    test.refreshComponent(true);
    const smartContractSpy = spyOn(test.component, 'loadSmartContract');
    test.fixture.detectChanges();
    expect(smartContractSpy).toHaveBeenCalled();
  });

  describe('on updating smart contract', () => {

    it('should reload the smart contract with the new version', () => {
      const response = {
        contract_id: 'contractId',
        version: 'newVersion',
        url: 'url'
      };
      const getVersionSpy = spyOn(test.component, 'getVersionInfo');
      const loadContractSpy = spyOn(test.component, 'loadSmartContract');

      test.component.afterUpdateContract(response);

      expect(getVersionSpy).toHaveBeenCalled();
      expect(loadContractSpy).toHaveBeenCalledWith(response.contract_id, response.version);
      expect(test.component.versionSelected).toBe(response.version);
    });
  });

});
