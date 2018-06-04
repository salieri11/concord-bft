/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnChanges, SimpleChanges, ViewChild } from '@angular/core';
import { EthSendCallParams, EthSendTransactionParams, SmartContractVersion } from '../../shared/remote-interfaces';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import * as Web3EthAbi from 'web3-eth-abi';
import { AthenaApiService } from '../../shared/athena-api.service';
import { EthApiService } from '../../shared/eth-api.service';
import { ContractPayloadPreviewModalComponent } from '../contract-payload-preview-modal/contract-payload-preview-modal.component';

@Component({
  selector: 'app-smart-contract-version-details',
  templateUrl: './smart-contract-version-details.component.html',
  styleUrls: ['./smart-contract-version-details.component.scss']
})
export class SmartContractVersionDetailsComponent implements OnChanges {

  @Input() version: SmartContractVersion;
  @ViewChild('payloadPreviewModal') payloadPreviewModal: ContractPayloadPreviewModalComponent;
  functions;
  versionForm: FormGroup;
  inputs = [];
  alertMessage: string;
  alertType: string;
  resultType: string;
  functionDefinition;

  constructor (private athenaApiService: AthenaApiService, private ethApiService: EthApiService) {
    this.versionForm = new FormGroup({
      functionName: new FormControl(''),
      contractForm: new FormGroup({
        gas:  new FormControl('', [Validators.required]),
        value: new FormControl(''),
        from: new FormControl('', [Validators.required]),
        functionInputs: new FormGroup({})
      })
    });
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.version) {
      this.functions = changes.version.currentValue.metadata.output.abi.filter(abi => abi.type === 'function');
      this.versionForm.reset();
      if (this.functions.length) {
        this.functionDefinition = this.functions[0];
        this.versionForm.patchValue({
          functionName: this.functionDefinition.name
        });
      } else {
        this.functionDefinition = undefined;
      }
    }
  }

  getFunctionDetails() {
    const result = this.functions.filter( func => func.name === this.versionForm.value.functionName);
    if (result.length > 0) {
      this.inputs = result[0].inputs;
      this.functionDefinition = result[0];
    } else {
      this.inputs = [];
      this.functionDefinition = null;
    }
    const inputFormGroup = new FormGroup({});
    for (let i = 0; i < this.inputs.length; i++) {
      inputFormGroup.addControl(this.inputs[i].name, new FormControl(''));
    }
    (this.versionForm.get('contractForm') as FormGroup).setControl('functionInputs', inputFormGroup);
    this.versionForm.get('contractForm').reset();
  }

  getType(controlType: string) {
    let type;
    switch (controlType) {
      case 'uint256':
      case 'uint8':
      case 'uint32':
      case 'uint16':
      case 'uint24':
      case 'int256':
      case 'int8':
      case 'int32':
      case 'int16':
      case 'int24':
      case 'uint':
      case 'int':
        type = 'number';
        break;
      case 'address':
        type = 'text';
        break;
      case 'string':
        type = 'text';
        break;
      default:
        type = 'text';
    }
    return type;
  }

  onSourceCodeDownload() {
    this.onDownload(this.version.sourcecode, 'sourceCode.sol');
  }

  onByteCodeDownload() {
    this.onDownload(this.version.bytecode, 'abiDownload.bin');
  }

  onMetadataDownload() {
    this.onDownload(JSON.stringify(this.version.metadata, null, 4), 'version_meta.json');
  }

  onDownload(source, file) {
    const a: HTMLAnchorElement = document.createElement('a');
    document.body.appendChild(a);
    a.style.display = 'none';

    const blob = new Blob([source], {type: 'octet/stream'});
    const url = window.URL.createObjectURL(blob);
    a.href = url;
    a.download = file;
    a.click();
    window.URL.revokeObjectURL(url);
  }

  onPreview() {
    this.payloadPreviewModal.open(JSON.stringify(this.encodeFunction(), null, 4));
  }

  onCall() {
    this.ethApiService.sendCall(this.encodeFunction()).subscribe((resp) => {
      if (resp.error) {
        this.handleError(resp);
      } else {
        this.alertMessage = resp.result;
        this.alertType = 'alert-success';
        this.resultType = 'call';
      }
    }, (errorResp) => {
      this.handleError(errorResp);
    });
  }

  onSend() {
    this.ethApiService.sendTransaction(this.encodeFunction()).subscribe((resp) => {
      console.log(resp);
      if (resp.error) {
        this.handleError(resp);
      } else {
        this.alertMessage = resp.result;
        this.alertType = 'alert-success';
        this.resultType = 'send';
      }
    }, (errorResp) => {
      this.handleError(errorResp);
    });
  }

  handleError(error) {
    this.alertMessage = error.error;
    this.alertType = 'alert-danger';
    this.resultType = 'error';
  }

  encodeFunction() {
    const paramsForm = this.versionForm.get('contractForm').get('functionInputs');
    const params = this.inputs.map(input => paramsForm.value[input.name]);
    const output = Web3EthAbi.encodeFunctionCall(this.functionDefinition, params);

    return {
      from: this.versionForm.value.contractForm.from,
      to: this.version.address,
      gas: this.versionForm.value.contractForm.gas,
      value: this.versionForm.value.contractForm.value,
      data: output
    };
  }
}
