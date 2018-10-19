/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, fakeAsync, tick, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClarityModule } from '@clr/angular';
import { ClrFormsNextModule } from '@clr/angular';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { of as observableOf, throwError } from 'rxjs';

import { MockSharedModule } from '../../shared/shared.module';
import { ContractFormComponent } from './contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { SmartContractsSolidityFunctionInputsComponent } from '../smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';


describe('ContractFormComponent', () => {
  let component: ContractFormComponent;
  let fixture: ComponentFixture<ContractFormComponent>;
  const testContract: any = {
    contract_id: 'contractId',
    owner: 'owner',
    versions: [{
      address: 'address',
      metadata: {},
      version: 'version',
      url: 'url'
    }]
  };
  const testVersion = {
    contract_id: 'contractId',
    version: 'version',
    owner: 'owner',
    metadata: {
      compiler: {
        version: 'version'
      },
      language: 'language',
      output: {
        abi: [{
          payable: false,
          inputs: [{name:"addr",type:"address"},{name:"title",type:"bytes1"}],
          outputs: [],
          stateMutability: "nonpayable",
          type: "constructor"
        }],
        devdoc: 'devdoc',
        userdoc: 'userdoc',
      },
      settings: 'settings',
      sources: 'sources',
      version: 1,
    },
    address: 'address',
    bytecode: 'bytecode',
    sourcecode: 'sourcecode'
  };
  const testExternalversion: any = {
    contract_id: 'contractId',
    version: 'version',
    owner: 'owner',
    metadata: {
      compiler: {
        version: 'version'
      },
      language: 'language',
      output: {
        abi: [],
        devdoc: 'devdoc',
        userdoc: 'userdoc',
      },
      settings: 'settings',
      sources: 'sources',
      version: 1,
    },
    address: 'address',
    bytecode: '',
    sourcecode: ''
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        ReactiveFormsModule,
        HttpClientTestingModule,
        MockSharedModule,
        BrowserAnimationsModule,
        FormsModule,
        ClrFormsNextModule
      ],
      declarations: [ContractFormComponent, SmartContractsSolidityFunctionInputsComponent],
      providers: [
        SmartContractsService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                ''
              ),
            },
          },
        }]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ContractFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On open create contract form', () => {
    it('should set isOpen to true', () => {
      expect(component.isOpen).toBeFalsy();
      component.open();
      expect(component.isOpen).toBeTruthy();
    });

    it('opens the create form when no smart contract is provided', () => {
      spyOn((component as any), 'createAddContractForm');
      component.open();
      expect((component as any).createAddContractForm).toHaveBeenCalled();
    });

    it('opens the update form when a smart contract is provided', () => {
      spyOn((component as any), 'createUpdateContractForm');
      component.open(testContract);
      expect((component as any).createUpdateContractForm).toHaveBeenCalled();
    });

    it('opens the update existing form when a version is provided without bytecode and sourcecode', () => {
      spyOn((component as any), 'createUpdateExternalContractForm').and.callThrough();
      component.open(testContract, testExternalversion);
      expect((component as any).createUpdateExternalContractForm).toHaveBeenCalled();
    });

    it('disables the contract id field and prepopulates the version field on edit', fakeAsync(() => {
      (component as any).createUpdateContractForm(testContract, testVersion);

      tick(20);
      fixture.detectChanges();

      fixture.whenStable().then(() => {
        expect(component.smartContractForm.controls['contractId'].disabled).toBe(true);
        expect(component.smartContractForm.getRawValue().contractId).toBe(testContract.contract_id);
        expect(component.smartContractForm.value.version).toBe(testVersion.version);
      });
    }));
  });

  describe('On closing the create contract form', () => {
    it('should set isOpen to false', () => {
      component.open();
      expect(component.isOpen).toBeTruthy();
      component.onClose();
      expect(component.isOpen).toBeFalsy();
    });
  });

  describe('On smart contract source code submission', () => {
    it('should call postSourceCode on source code submission', () => {
      const spy = spyOn((component as any).smartContractsService, 'postSourceCode').and.returnValue(observableOf({data: [{contract_name: 'contractName'}]}));
      const sourceCode = {
        sourcecode: component.smartContractForm.value.file
      };

      component.onSubmitSourceCode();
      expect(spy).toHaveBeenCalledWith(sourceCode);
    });

    it('should show an error if code submission fails', () => {
      const spy = spyOn((component as any).smartContractsService, 'postSourceCode').and.returnValue(throwError({ error: 'error' }));
      const sourceCode = {
        sourcecode: component.smartContractForm.value.file
      };

      component.onSubmitSourceCode();
      expect(spy).toHaveBeenCalledWith(sourceCode);
      expect(component.modalState.error).toBeTruthy();
    });
  });

  describe('On smart contract selection', () => {
    it('should select the appropriate constructorAbi', () => {
      component.multiContractResponse = [{contract_name: 'TestContract', metadata: testVersion.metadata}];
      component.contractsForm.controls['selectedContract'].setValue('TestContract');
      component.onSelectContract();
      expect(component.constructorAbi).toBe(testVersion.metadata.output.abi[0]);
    });
  });

  describe('Parameter encoding', () => {
    it('should correctly encode the constructorAbi parameters', () => {
      component.multiContractResponse = [{contract_name: 'TestContract', metadata: testVersion.metadata}];
      component.contractsForm.controls['selectedContract'].setValue('TestContract');
      component.onSelectContract();

      component.constructorParamsForm.controls['addr'].setValue('0x166c22687709b3273097283a73daf33bed4bb252');
      component.constructorParamsForm.controls['title'].setValue('a');

      const result = (component as any).encodeConstructorParams();

      expect(result).toBe('000000000000000000000000166c22687709b3273097283a73daf33bed4bb2526100000000000000000000000000000000000000000000000000000000000000');
    });
  });

  describe('On smart contract submission', () => {
    it('should call postSmartContract if the modalState.isUpdateExternal is false', () => {
      const spy = spyOn(component, 'postSmartContract');
      expect(component.modalState.isUpdateExternal).toBe(false);
      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalled();
    });

    it('should call updateExistingSmartContract if the modalState.isUpdateExternal is true', () => {
      const spy = spyOn(component, 'updateExistingSmartContract');
      component.modalState.isUpdateExternal = true;
      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalled();
    });

    it('should emit the contractCreated event on update success', () => {
      component.modalState.isUpdateExternal = true;
      component.version = testVersion;
      const spy = spyOn((component as any).smartContractsService, 'updateExistingVersion').and.returnValue(observableOf(true));
      const contract = {
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file,
        contractName: component.contractsForm.value.selectedContract,
        constructorParams: '',
        existingVersionName: testVersion.version,
        existingContractId: testVersion.contract_id
      };
      const eventSpy = spyOn(component.contractCreated, 'emit');
      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalledWith(testVersion.contract_id, testVersion.version, contract);
      expect(eventSpy).toHaveBeenCalled();
    });

    it('should generate an error when update of smart contract version fails', () => {
      component.modalState.isUpdateExternal = true;
      component.version = testVersion;
      const spy = spyOn((component as any).smartContractsService, 'updateExistingVersion').and.returnValue(throwError({ error: 'error' }));
      const contract = {
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file,
        contractName: component.contractsForm.value.selectedContract,
        constructorParams: '',
        existingVersionName: testVersion.version,
        existingContractId: testVersion.contract_id
      };

      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalledWith(testVersion.contract_id, testVersion.version, contract);
      expect(component.modalState.error).toBeTruthy();
    });

    it('should emit contract created event on submit success', () => {
      const spy = spyOn((component as any).smartContractsService, 'postContract').and.returnValue(observableOf(true));
      const contract = {
        id: 1,
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file,
        contractName: component.contractsForm.value.selectedContract,
        constructorParams: ''
      };
      const eventSpy = spyOn(component.contractCreated, 'emit');

      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalledWith(contract);
      expect(eventSpy).toHaveBeenCalled();
    });

    it('should generate error when creation of smart contract fails', () => {
      const spy = spyOn((component as any).smartContractsService, 'postContract').and.returnValue(throwError({ error: 'error' }));
      const contract = {
        id: 1,
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file,
        contractName: component.contractsForm.value.selectedContract,
        constructorParams: ''
      };

      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalledWith(contract);
      expect(component.modalState.error).toBeTruthy();
    });

    it('when smart contract creation fails, errorMessage should contain the error returned from response', () => {
      const spy = spyOn((component as any).smartContractsService, 'postContract').and.returnValue(observableOf({ error: 'error' }));
      const contract = {
        id: 1,
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file,
        contractName: component.contractsForm.value.selectedContract,
        constructorParams: ''
      };

      component.onSubmitSmartContract();
      expect(spy).toHaveBeenCalledWith(contract);
      expect(component.modalState.errorMessage).toEqual('error');
    });

    describe('On smart contract file upload', () => {
      it('should read the uploaded smart contract file', (done) => {
        const fileContents = 'string text';
        const event: any = {
          target: {
            files: [new File([fileContents], 'filename')]
          }
        };

        component.onSmartContractFileChange(event);
        setTimeout(() => {
          expect(component.smartContractForm.value.file).toEqual(fileContents);
          done();
        }, 200);
      });

      it('should set file to null if no file is uploaded', (done) => {
        const event: any = {
          target: {
            files: []
          }
        };

        component.onSmartContractFileChange(event);
        setTimeout(() => {
          expect(component.smartContractForm.value.file).toEqual(null);
          done();
        }, 200);
      });

    });

  });

});
