/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, ViewChild } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { of as observableOf, throwError } from 'rxjs';
import { ClrFormsNextModule } from '@clr/angular';
import { RouterTestingModule } from '@angular/router/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { SmartContractVersionComponent } from './smart-contract-version.component';
import {
  SmartContractsSolidityFunctionInputsComponent
} from '../smart-contracts-solidity-function-inputs/smart-contracts-solidity-function-inputs.component';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';
import { SmartContractVersion } from '../shared/smart-contracts.model';

const initialVersion = {
  contract_id: 'id',
  version: 'version',
  owner: 'owner',
  metadata: {
    compiler: {
      version: 'version'
    },
    language: 'language',
    output: {
      abi: [],
      devdoc: {},
      userdoc: {},
    },
    settings: {},
    sources: {},
    version: 1
  },
  address: 'address',
  bytecode: 'bytecode',
  sourcecode: 'sourcecode'
};

@Component({
  selector: 'athena-test-wrapper',
  template: `
        <athena-smart-contract-version #versionComponent [version]="version"></athena-smart-contract-version>
    `
})
class TestWrapperClassComponent {
  @ViewChild('versionComponent') versionComponent: SmartContractVersionComponent;
  version: SmartContractVersion = initialVersion;
}

describe('SmartContractVersionComponent', () => {
  let component: TestWrapperClassComponent;
  let fixture: ComponentFixture<TestWrapperClassComponent>;
  const nextVersion: SmartContractVersion = {
    contract_id: 'id2',
    version: 'version2',
    owner: 'owner2',
    metadata: {
      compiler: {
        version: 'version2'
      },
      language: 'language2',
      output: {
        abi: [{
          type: 'function',
          name: 'function',
          constant: true,
          payable: true,
          stateMutability: 'stateMutability',
          inputs: [{type: 'uint', name: 'input'}],
          outputs: [{type: 'uint', name: 'output'}],
        }],
        devdoc: {},
        userdoc: {},
      },
      settings: {},
      sources: {},
      version: 2
    },
    address: 'address2',
    bytecode: 'bytecode2',
    sourcecode: 'sourcecode2'
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        HttpClientTestingModule,
        FormsModule,
        ClrFormsNextModule
      ],
      declarations: [
        SmartContractVersionComponent,
        ContractPayloadPreviewFormComponent,
        SmartContractsSolidityFunctionInputsComponent,
        TestWrapperClassComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TestWrapperClassComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On changes', () => {
    it('calls onVersionChange', () => {
      spyOn((component.versionComponent as any), 'onVersionChange');
      component.version = nextVersion;
      fixture.detectChanges();

      expect((component.versionComponent as any).onVersionChange).toHaveBeenCalled();
    });

    it('resets the version form', () => {
      spyOn(component.versionComponent.versionForm, 'reset');
      component.version = nextVersion;
      fixture.detectChanges();

      expect(component.versionComponent.versionForm.reset).toHaveBeenCalled();
    });
    it('sets inputs and function definition when functions are present', () => {
      spyOn((component.versionComponent as any), 'onVersionChange').and.callThrough();
      expect(component.versionComponent.inputs).toEqual([]);
      expect(component.versionComponent.functionDefinition).toBeFalsy();
      component.version = nextVersion;
      fixture.detectChanges();

      expect((component.versionComponent as any).onVersionChange).toHaveBeenCalled();
      expect(component.versionComponent.inputs.length).toEqual(1);
      expect(component.versionComponent.functionDefinition).toBeTruthy();
    });
    it('empties inputs and function definition when functions are not present', () => {
      spyOn((component.versionComponent as any), 'onVersionChange').and.callThrough();
      expect(component.versionComponent.inputs).toEqual([]);
      expect(component.versionComponent.functionDefinition).toBeFalsy();
      component.version = {...initialVersion};
      fixture.detectChanges();

      expect((component.versionComponent as any).onVersionChange).toHaveBeenCalled();
      expect(component.versionComponent.inputs).toEqual([]);
      expect(component.versionComponent.functionDefinition).toBeFalsy();
    });
  });

  describe('Download functions', () => {
    it('generates the source code file name properly', () => {
      const expectedFileName = `${initialVersion.contract_id}_${initialVersion.version}_source_code.sol`;
      spyOn((component.versionComponent as any), 'onDownload');
      component.versionComponent.onSourceCodeDownload();

      expect((component.versionComponent as any).onDownload)
        .toHaveBeenCalledWith(initialVersion.sourcecode, expectedFileName);
    });
    it('generates the byte code file name properly', () => {
      const expectedFileName = `${initialVersion.contract_id}_${initialVersion.version}_bytecode.bin`;
      spyOn((component.versionComponent as any), 'onDownload');
      component.versionComponent.onByteCodeDownload();

      expect((component.versionComponent as any).onDownload)
        .toHaveBeenCalledWith(initialVersion.bytecode, expectedFileName);
    });
    it('generates the metadata file name properly', () => {
      const expectedFileName = `${initialVersion.contract_id}_${initialVersion.version}_metadata.json`;
      spyOn((component.versionComponent as any), 'onDownload');
      component.versionComponent.onMetadataDownload();

      expect((component.versionComponent as any).onDownload)
        .toHaveBeenCalledWith(JSON.stringify(initialVersion.metadata, null, 4), expectedFileName);
    });
    it('generates a source code download link', () => {
      const createUrlSpy = spyOn(window.URL, 'createObjectURL').and.callThrough();
      const revokeUrlSpy = spyOn(window.URL, 'revokeObjectURL').and.callThrough();
      const body = document.querySelector('body');
      let linkClicked = false;
      body.addEventListener('click', (e) => {
        e.preventDefault();
        linkClicked = true;
      }, {once: true});

      component.versionComponent.onSourceCodeDownload();

      expect(linkClicked).toBe(true);
      expect(createUrlSpy).toHaveBeenCalled();
      expect(revokeUrlSpy).toHaveBeenCalled();
    });

    it('generates a bytecode download link', () => {
      const createUrlSpy = spyOn(window.URL, 'createObjectURL').and.callThrough();
      const revokeUrlSpy = spyOn(window.URL, 'revokeObjectURL').and.callThrough();
      const body = document.querySelector('body');
      let linkClicked = false;
      body.addEventListener('click', (e) => {
        e.preventDefault();
        linkClicked = true;
      }, {once: true});

      component.versionComponent.onByteCodeDownload();

      expect(linkClicked).toBe(true);
      expect(createUrlSpy).toHaveBeenCalled();
      expect(revokeUrlSpy).toHaveBeenCalled();
    });

    it('generates a metadata download link', () => {
      const createUrlSpy = spyOn(window.URL, 'createObjectURL').and.callThrough();
      const revokeUrlSpy = spyOn(window.URL, 'revokeObjectURL').and.callThrough();
      const body = document.querySelector('body');
      let linkClicked = false;
      body.addEventListener('click', (e) => {
        e.preventDefault();
        linkClicked = true;
      }, {once: true});

      component.versionComponent.onMetadataDownload();

      expect(linkClicked).toBe(true);
      expect(createUrlSpy).toHaveBeenCalled();
      expect(revokeUrlSpy).toHaveBeenCalled();
    });
  });

  describe('Preview', () => {
    it('calls open on the preview modal and passes the encoded function', () => {
      const modalSpy = spyOn(component.versionComponent.payloadPreviewModal, 'open');
      const encodeSpy = spyOn((component.versionComponent as any), 'encodeFunction');

      component.versionComponent.onPreview();

      expect(modalSpy).toHaveBeenCalled();
      expect(encodeSpy).toHaveBeenCalled();
    });
  });

  describe('API interaction', () => {
    it('on call passes the encoded function and handles success', () => {
      const successResult = 'smartContracts.form.callSuccessMessage';
      const expectedPayload = {
        from: null,
        to: 'address2',
        gas: '0xF4240',
        data: '0x4ad12c050000000000000000000000000000000000000000000000000000000000000000'
      };
      const callSpy = spyOn((component.versionComponent as any).ethApiService, 'sendCall')
        .and.returnValue(observableOf({result: successResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onCall();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(component.versionComponent.alertMessage).toBe(successResult);
      expect(component.versionComponent.alertType).toBe('alert-success');
      expect(component.versionComponent.resultType).toBe('call');
    });
    it('handles call errors in success responses', () => {
      const errorResult = 'error';
      spyOn((component.versionComponent as any).ethApiService, 'sendCall')
        .and.returnValue(observableOf({error: errorResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onCall();

      expect(component.versionComponent.alertMessage).toBe(errorResult);
      expect(component.versionComponent.alertType).toBe('alert-danger');
      expect(component.versionComponent.resultType).toBe('error');
    });
    it('handles call errors in error responses', () => {
      const errorResult = 'error';
      spyOn((component.versionComponent as any).ethApiService, 'sendCall')
        .and.returnValue(throwError({error: errorResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onCall();

      expect(component.versionComponent.alertMessage).toBe(errorResult);
      expect(component.versionComponent.alertType).toBe('alert-danger');
      expect(component.versionComponent.resultType).toBe('error');
    });

    it('sends a transaction with the encoded function', () => {
      const successResult = 'success result';
      const expectedPayload = {
        from: null,
        to: 'address2',
        gas: '0xF4240',
        data: '0x4ad12c050000000000000000000000000000000000000000000000000000000000000000'
      };
      const callSpy = spyOn((component.versionComponent as any).ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({result: successResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onSend();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(component.versionComponent.alertMessage).toBe(successResult);
      expect(component.versionComponent.alertType).toBe('alert-success');
      expect(component.versionComponent.resultType).toBe('send');
    });
    it('handles transaction errors in success responses', () => {
      const errorResult = 'error';
      spyOn((component.versionComponent as any).ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({error: errorResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onSend();

      expect(component.versionComponent.alertMessage).toBe(errorResult);
      expect(component.versionComponent.alertType).toBe('alert-danger');
      expect(component.versionComponent.resultType).toBe('error');
    });
    it('handles transaction errors in error responses', () => {
      const errorResult = 'error';
      spyOn((component.versionComponent as any).ethApiService, 'sendTransaction')
        .and.returnValue(throwError({error: errorResult}));

      component.version = nextVersion;
      fixture.detectChanges();
      component.versionComponent.onSend();

      expect(component.versionComponent.alertMessage).toBe(errorResult);
      expect(component.versionComponent.alertType).toBe('alert-danger');
      expect(component.versionComponent.resultType).toBe('error');
    });

  });
});
