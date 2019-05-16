/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { of as observableOf } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';

import { SmartContractComponent } from './smart-contract.component';
import { SmartContractVersionComponent } from '../smart-contract-version/smart-contract-version.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from '../smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';
import { ContractFormComponent } from '../contract-form/contract-form.component';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { TransactionDetailsComponent } from '../../transactions/transaction-details/transaction-details.component';

import { SmartContractsService } from '../shared/smart-contracts.service';
import { TourService } from '../../shared/tour.service';
import { CodeHighlighterComponent } from '../../shared/components/code-highlighter/code-highlighter.component';

describe('SmartContractComponent', () => {
  let component: SmartContractComponent;
  let fixture: ComponentFixture<SmartContractComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        RouterTestingModule,
        FormsModule,
        HttpClientTestingModule,
      ],
      declarations: [
        SmartContractComponent,
        SmartContractVersionComponent,
        ContractFormComponent,
        ContractPayloadPreviewFormComponent,
        SmartContractsSolidityFunctionInputsComponent,
        TransactionDetailsComponent,
        VmwCopyToClipboardButtonComponent,
        CodeHighlighterComponent
      ],
      providers: [
        SmartContractsService,
        TourService,
        NgxTourService,
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: {parent: {parent: {params: {consortiumId: '1234'}}}},
            params: observableOf({ contractId: '2', version: '1' }),
            fragment: observableOf('')
          }
        }
      ]
    })
      .compileComponents();

  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load smart contract with given contractId', () => {
    const spy = spyOn((component as any).smartContractsService, 'getSmartContract')
      .and.returnValue(observableOf({contract_id: 'smart contract', versions: []}));
    component.loadSmartContract('contractId');
    expect(spy).toHaveBeenCalled();
  });

  it('should load version details for given contractId and version', () => {
    const spy = spyOn((component as any).smartContractsService, 'getVersionDetails')
      .and.returnValue(observableOf({ versionDetails: 'version details' }));
    component.loadVersionDetails('contractId', 'version');
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

    const spy = spyOn(component, 'getVersionInfo');
    spyOn((component as any).smartContractsService, 'getSmartContract').and.returnValue(observableOf(contract));

    component.loadSmartContract('contractId');

    expect(component.versionSelected).toBe(contract.versions[0].version);
    expect(spy).toHaveBeenCalled();
  });

  it('should navigate to the version details page', () => {
    const spy = spyOn((component as any).router, 'navigate');
    component.smartContract = {
      contract_id: '1',
      owner: 'owner',
      versions: undefined
    };

    component.versionSelected = 1;
    component.getVersionInfo();
    expect(spy).toHaveBeenCalledWith(['1234', 'smart-contracts', '1', 'versions', 1], Object({ replaceUrl: true }));
  });

  describe('should load smart contract with contractId in the params', () => {

    it('should load smartContract with contractId in the params', () => {
      const smartContractSpy = spyOn(component, 'loadSmartContract');
      component.smartContract = {
        contract_id: '1',
        owner: 'owner',
        versions: undefined
      };

      component.loadSmartContract('2');
      expect(smartContractSpy).toHaveBeenCalled();
    });
  });

  describe('on updating smart contract', () => {

    it('should reload the smart contract with the new version', () => {
      const response = {
        contract_id: 'contractId',
        version: 'newVersion',
        url: 'url'
      };
      const getVersionSpy = spyOn(component, 'getVersionInfo');
      const loadContractSpy = spyOn(component, 'loadSmartContract');

      component.afterUpdateContract(response);

      expect(getVersionSpy).toHaveBeenCalled();
      expect(loadContractSpy).toHaveBeenCalledWith(response.contract_id, response.version);
      expect(component.versionSelected).toBe(response.version);
    });
  });

});
