/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ClarityModule } from '@clr/angular';
import { ReactiveFormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { ContractFormComponent } from './contract-form.component';
import { SmartContractsService } from "../shared/smart-contracts.service";
import { of as observableOf, throwError } from "rxjs";


fdescribe('ContractFormComponent', () => {
  let component: ContractFormComponent;
  let fixture: ComponentFixture<ContractFormComponent>;
  let service: SmartContractsService;
  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        ReactiveFormsModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [ContractFormComponent],
      providers: [SmartContractsService]
    })
      .compileComponents();
    service = TestBed.get(SmartContractsService);
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
    it('resets the form', () => {
      spyOn(component.smartContractForm, 'reset');
      component.open();
      expect(component.smartContractForm.reset).toHaveBeenCalled();
    });

    it('should set isOpen to true', () => {
      expect(component.isOpen).toBeFalsy();
      component.open();
      expect(component.isOpen).toBeTruthy();
    });
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
      const spy = spyOn(service, 'postContract').and.returnValue(observableOf(true));
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
      const spy = spyOn(service, 'postContract').and.returnValue(throwError({ error: 'error' }));
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
      const spy = spyOn(service, 'postContract').and.returnValue(observableOf({ error: 'error' }));
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
