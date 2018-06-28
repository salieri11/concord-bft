/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Component, Input, OnChanges, SimpleChanges, SimpleChange, ViewChild } from '@angular/core';

import { SmartContractVersion } from '../../shared/remote-interfaces';
import * as Web3EthAbi from 'web3-eth-abi';

import { EthApiService } from '../../shared/eth-api.service';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';
import { isHexAddress, isHexadecimal } from '../shared/custom-validators';

@Component({
  selector: 'app-smart-contract-version',
  templateUrl: './smart-contract-version.component.html',
  styleUrls: ['./smart-contract-version.component.scss']
})
export class SmartContractVersionComponent implements OnChanges {

  @Input() version: SmartContractVersion;
  @ViewChild('payloadPreviewModal') payloadPreviewModal: ContractPayloadPreviewFormComponent;
  functions;
  versionForm: FormGroup;
  inputs = [];
  alertMessage: string;
  alertType: string;
  resultType: string;
  functionDefinition;

  constructor(private ethApiService: EthApiService) {
    this.versionForm = new FormGroup({
      functionName: new FormControl(''),
      contractForm: new FormGroup({
        gas: new FormControl('', [Validators.required, isHexadecimal]),
        value: new FormControl('', [isHexadecimal]),
        from: new FormControl('', [Validators.required, isHexAddress]),
        functionInputs: new FormGroup({})
      })
    });
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.version) {
      this.onVersionChange(changes.version);
    }
  }

  getFunctionDetails() {
    (this.versionForm.get('contractForm') as FormGroup).setControl('functionInputs', new FormGroup({}));
    this.versionForm.get('contractForm').reset();
    const result = this.functions.filter(func => func.name === this.versionForm.value.functionName);
    if (result.length > 0) {
      this.inputs = result[0].inputs;
      this.functionDefinition = result[0];
    } else {
      this.inputs = [];
      this.functionDefinition = null;
    }
  }

  onSourceCodeDownload() {
    this.onDownload(this.version.sourcecode, `${this.generateFileName()}_source_code.sol`);
  }

  onByteCodeDownload() {
    this.onDownload(this.version.bytecode, `${this.generateFileName()}_bytecode.bin`);
  }

  onMetadataDownload() {
    this.onDownload(JSON.stringify(this.version.metadata, null, 4), `${this.generateFileName()}_metadata.json`);
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
    }, errorResp => this.handleError(errorResp));
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
    }, errorResp => this.handleError(errorResp));
  }

  private encodeFunction() {
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

  private generateFileName() {
    return `${this.version.contract_id}_${this.version.version}`;
  }

  private handleError(error) {
    this.alertMessage = error.error;
    this.alertType = 'alert-danger';
    this.resultType = 'error';
  }

  private onDownload(source, file) {
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

  private onVersionChange(version: SimpleChange) {
    this.functions = version.currentValue.metadata.output.abi.filter(abi => abi.type === 'function');
    this.versionForm.reset();
    this.versionForm.value.contractForm.functionInputs = new FormGroup({});
    if (this.functions.length) {
      this.functionDefinition = this.functions[0];
      this.versionForm.patchValue({
        functionName: this.functionDefinition.name
      });
    } else {
      this.functionDefinition = undefined;
    }
    this.getFunctionDetails();
  }
}
