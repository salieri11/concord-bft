/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { of as observableOf, throwError } from 'rxjs';
import { mockSmartContractVersion } from '../shared/smart-contracts.model';
import * as DownloadHelpers from '../../shared/download-helpers';
import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
import { SmartContractVersionComponent } from './smart-contract-version.component';
import { EthApiService } from '../../shared/eth-api.service';
import { ClarityModule } from '@clr/angular';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';

describe('SmartContractVersionComponent', () => {
  let ethApiService: EthApiService;
  const mockVersion = mockSmartContractVersion;
  const test = testFor(SmartContractVersionComponent).expedite({
    imports: [ClarityModule],
    provides: [],
    declarations: [SmartContractVersionComponent, ContractPayloadPreviewFormComponent],
  }, beforeTesting(() => {
    ethApiService = test.getService(EthApiService);
  }), prepareEach(() => {
    test.component.version = mockVersion;
    test.component.onVersionChange(mockVersion);
  }));


  it('should create', () => {
    expect(test.component).toBeTruthy();
  });


  describe('Download functions', () => {
    it('generates the source code file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_source_code.sol`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);

      test.component.onSourceCodeDownload();

      expect(DownloadHelpers.generateDownload)
        .toHaveBeenCalledWith(expectedFileName, mockVersion.sourcecode);
    });
    it('generates the byte code file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_bytecode.bin`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);
      test.component.onByteCodeDownload();

      expect(DownloadHelpers.generateDownload)
        .toHaveBeenCalledWith(expectedFileName, mockVersion.bytecode);
    });
    it('generates the metadata file name properly', () => {
      const expectedFileName = `${mockVersion.contract_id}_${mockVersion.version}_metadata.json`;
      const setPropertyTypeSpy = jasmine.createSpy('setPropertyTypeSpy');
      spyOnProperty(DownloadHelpers, 'generateDownload').and.returnValue(setPropertyTypeSpy);
      test.component.onMetadataDownload();

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

      test.component.onSourceCodeDownload();

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

      test.component.onByteCodeDownload();

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

      test.component.onMetadataDownload();

      expect(linkClicked).toBe(true);
      expect(createUrlSpy).toHaveBeenCalled();
      expect(revokeUrlSpy).toHaveBeenCalled();
    });
  });

  describe('Preview', () => {
    it('calls open on the preview modal and passes the encoded function', () => {
      const modalSpy = spyOn(test.component.payloadPreviewModal, 'open');
      const encodeSpy = spyOn((test.component as any), 'encodeFunction');

      test.component.onPreview();

      expect(modalSpy).toHaveBeenCalled();
      expect(encodeSpy).toHaveBeenCalled();
    });
  });

  describe('API interaction', () => {
    it('on call passes the encoded function and handles success', () => {
      const successResult = 'smartContracts.form.callSuccessMessage';
      const expectedPayload = {
        from: '',
        to: 'address2',
        gas: '0xF4240',
        data: ''
      };
      const callSpy = spyOn(ethApiService, 'sendCall')
        .and.returnValue(observableOf({result: '0x0000000000000000000000000000000000000000000000000000000000000000'}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onCall();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(test.component.alertMessage).toBe(successResult);
      expect(test.component.alertType).toBe('alert-success');
      expect(test.component.resultType).toBe('call');
    });
    it('handles call errors in success responses', () => {
      const errorResult = 'error';
      spyOn(ethApiService, 'sendCall')
        .and.returnValue(observableOf({error: errorResult}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onCall();

      expect(test.component.alertMessage).toBe(errorResult);
      expect(test.component.alertType).toBe('alert-danger');
      expect(test.component.resultType).toBe('error');
    });
    it('handles call errors in error responses', () => {
      const errorResult = 'error';
      spyOn(ethApiService, 'sendCall')
        .and.returnValue(throwError({error: errorResult}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onCall();

      expect(test.component.alertMessage).toBe(errorResult);
      expect(test.component.alertType).toBe('alert-danger');
      expect(test.component.resultType).toBe('error');
    });

    it('sends a transaction with the encoded function', () => {
      const successResult = 'success result';
      const expectedPayload = {
        from: '',
        to: 'address2',
        gas: '0xF4240',
        data: ''
      };
      const callSpy = spyOn(ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({result: successResult}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onSend();

      expect(callSpy).toHaveBeenCalledWith(expectedPayload);
      expect(test.component.alertMessage).toBe(successResult);
      expect(test.component.alertType).toBe('alert-success');
      expect(test.component.resultType).toBe('send');
    });
    it('handles transaction errors in success responses', () => {
      const errorResult = 'error';
      spyOn(ethApiService, 'sendTransaction')
        .and.returnValue(observableOf({error: errorResult}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onSend();

      expect(test.component.alertMessage).toBe(errorResult);
      expect(test.component.alertType).toBe('alert-danger');
      expect(test.component.resultType).toBe('error');
    });
    it('handles transaction errors in error responses', () => {
      const errorResult = 'error';
      spyOn(ethApiService, 'sendTransaction')
        .and.returnValue(throwError({error: errorResult}));

      test.refreshComponent();
      test.component.version = mockSmartContractVersion;
      test.fixture.detectChanges();
      test.component.onSend();

      expect(test.component.alertMessage).toBe(errorResult);
      expect(test.component.alertType).toBe('alert-danger');
      expect(test.component.resultType).toBe('error');
    });

  });
});
