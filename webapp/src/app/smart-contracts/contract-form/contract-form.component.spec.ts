/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, fakeAsync, tick, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClarityModule } from '@clr/angular';
import { ReactiveFormsModule } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { of as observableOf, throwError } from 'rxjs';

import { MockSharedModule } from '../../shared/shared.module';
import { ContractFormComponent } from './contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';


describe('ContractFormComponent', () => {
  let component: ContractFormComponent;
  let fixture: ComponentFixture<ContractFormComponent>;
  const contract: any = {
    contract_id: 'contractId',
    owner: 'owner',
    versions: [{
      address: 'address',
      metadata: {},
      version: 'version',
      url: 'url'
    }]
  };
  const version = {
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
      version: 'version',
    },
    address: 'address',
    bytecode: 'bytecode',
    sourcecode: 'sourcecode'
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        ReactiveFormsModule,
        HttpClientTestingModule,
        MockSharedModule,
        BrowserAnimationsModule
      ],
      declarations: [ContractFormComponent],
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
      component.open(contract);
      expect((component as any).createUpdateContractForm).toHaveBeenCalled();
    });

    it('disables the contract id field and prepopulates the version field on edit', fakeAsync(() => {
      (component as any).createUpdateContractForm(contract, version);

      tick(20);
      fixture.detectChanges();

      fixture.whenStable().then(() => {
        expect(component.smartContractForm.controls['contractId'].disabled).toBe(true);
        expect(component.smartContractForm.getRawValue().contractId).toBe(contract.contract_id);
        expect(component.smartContractForm.value.version).toBe(version.version);
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

  describe('On smart contract submission', () => {
    it('should emit contract created event on submit success', () => {
      const spy = spyOn((component as any).smartContractsService, 'postContract').and.returnValue(observableOf(true));
      const contract = {
        id: 1,
        from: component.smartContractForm.value.from,
        contract_id: component.smartContractForm.value.contractId,
        version: component.smartContractForm.value.version,
        sourcecode: component.smartContractForm.value.file
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
        sourcecode: component.smartContractForm.value.file
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
        sourcecode: component.smartContractForm.value.file
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
