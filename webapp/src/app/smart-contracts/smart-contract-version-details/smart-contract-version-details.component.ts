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
  contractForm: FormGroup;
  inputs = [];
  functionName;
  functionDefinition;

  constructor (private athenaApiService: AthenaApiService, private ethApiService: EthApiService) {
    this.contractForm = new FormGroup({
      gas:  new FormControl('', [Validators.required]),
      value: new FormControl(''),
      from: new FormControl('', [Validators.required]),
      functionInputs: new FormGroup({})
    });
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.version) {
      this.functions = changes.version.currentValue.metadata.output.abi.filter(abi => abi.type === 'function');
      this.contractForm.reset();
      if (this.functions.length) {
        this.functionDefinition = this.functions[0];
        this.functionName = this.functionDefinition.name;
      } else {
        this.functionDefinition = undefined;
        this.functionName = undefined;
      }
    }
  }

  getFunctionDetails() {
    const result = this.functions.filter( func => func.name === this.functionName);
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
    this.contractForm.setControl('functionInputs', inputFormGroup);
    this.contractForm.reset();
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
    this.onDownload(this.version.bytecode, 'abiDownload.sol');
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
      console.log(resp);
    });
  }

  onSend() {
    this.ethApiService.sendTransaction(this.encodeFunction()).subscribe((resp) => {
      console.log(resp);
    });
  }

  encodeFunction() {
    const paramsForm = this.contractForm.get('functionInputs');

    const params = this.inputs.map(input => paramsForm.value[input.name]);

    const output = Web3EthAbi.encodeFunctionCall(this.functionDefinition, params);

    return {
      from: this.contractForm.value.from,
      to: this.version.address,
      gas: this.contractForm.value.gas,
      value: this.contractForm.value.value,
      data: output
    };
  }
}
