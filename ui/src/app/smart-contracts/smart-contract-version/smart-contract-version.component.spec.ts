/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { of as observableOf, throwError } from 'rxjs';

import { SmartContractVersionComponent } from './smart-contract-version.component';
import { SmartContractVersion } from '../shared/smart-contracts.model';
import * as DownloadHelpers from '../../shared/download-helpers';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('SmartContractVersionComponent', () => {
  let component: SmartContractVersionComponent;
  let fixture: ComponentFixture<SmartContractVersionComponent>;
  const mockVersion: SmartContractVersion = {
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

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractVersionComponent);
    component = fixture.componentInstance;
    component.version = mockVersion;
    component.onVersionChange(mockVersion);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  describe('Download functions', () => {
    it('generates the source code file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_source_code.sol`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);

      component.onSourceCodeDownload();

      expect(DownloadHelpers.generateDownload)
        .toHaveBeenCalledWith(expectedFileName, mockVersion.sourcecode);
    });
    it('generates the byte code file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_bytecode.bin`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);
      component.onByteCodeDownload();

      expect(DownloadHelpers.generateDownload)
        .toHaveBeenCalledWith(expectedFileName, mockVersion.bytecode);
    });
    it('generates the metadata file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_metadata.json`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);
      component.onMetadataDownload();

      expect(DownloadHelpers.generateDownload)
        .toHaveBeenCalledWith(expectedFileName, JSON.stringify(mockVersion.metadata, null, 4));
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

      component.onSourceCodeDownload();

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

      component.onByteCodeDownload();

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

      component.onMetadataDownload();

      expect(linkClicked).toBe(true);
      expect(createUrlSpy).toHaveBeenCalled();
      expect(revokeUrlSpy).toHaveBeenCalled();
    });
  });

  describe('Preview', () => {
    it('calls open on the preview modal and passes the encoded function', () => {
      const modalSpy = spyOn(component.payloadPreviewModal, 'open');
      const encodeSpy = spyOn((component as any), 'encodeFunction');

      component.onPreview();

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
        data: ''
      };
      const callSpy = spyOn((component as any).ethApiService, 'sendCall')
        .and.returnValue(observableOf({result: '0x0000000000000000000000000000000000000000000000000000000000000000'}));

      fixture.detectChanges();
      component.onCall();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(component.alertMessage).toBe(successResult);
      expect(component.alertType).toBe('alert-success');
      expect(component.resultType).toBe('call');
    });
    it('handles call errors in success responses', () => {
      const errorResult = 'error';
      spyOn((component as any).ethApiService, 'sendCall')
        .and.returnValue(observableOf({error: errorResult}));

      fixture.detectChanges();
      component.onCall();

      expect(component.alertMessage).toBe(errorResult);
      expect(component.alertType).toBe('alert-danger');
      expect(component.resultType).toBe('error');
    });
    it('handles call errors in error responses', () => {
      const errorResult = 'error';
      spyOn((component as any).ethApiService, 'sendCall')
        .and.returnValue(throwError({error: errorResult}));

      fixture.detectChanges();
      component.onCall();

      expect(component.alertMessage).toBe(errorResult);
      expect(component.alertType).toBe('alert-danger');
      expect(component.resultType).toBe('error');
    });

    it('sends a transaction with the encoded function', () => {
      const successResult = 'success result';
      const expectedPayload = {
        from: null,
        to: 'address2',
        gas: '0xF4240',
        data: ''
      };
      const callSpy = spyOn((component as any).ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({result: successResult}));

      fixture.detectChanges();
      component.onSend();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(component.alertMessage).toBe(successResult);
      expect(component.alertType).toBe('alert-success');
      expect(component.resultType).toBe('send');
    });
    it('handles transaction errors in success responses', () => {
      const errorResult = 'error';
      spyOn((component as any).ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({error: errorResult}));

      fixture.detectChanges();
      component.onSend();

      expect(component.alertMessage).toBe(errorResult);
      expect(component.alertType).toBe('alert-danger');
      expect(component.resultType).toBe('error');
    });
    it('handles transaction errors in error responses', () => {
      const errorResult = 'error';
      spyOn((component as any).ethApiService, 'sendTransaction')
        .and.returnValue(throwError({error: errorResult}));

      fixture.detectChanges();
      component.onSend();

      expect(component.alertMessage).toBe(errorResult);
      expect(component.alertType).toBe('alert-danger');
      expect(component.resultType).toBe('error');
    });

  });
});
