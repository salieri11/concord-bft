/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { FormControl, FormGroup, Validators } from '@angular/forms';
import {
  Component,
  Input,
  OnChanges,
  SimpleChanges,
  SimpleChange,
  ViewChild,
  OnInit
} from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { ActivatedRoute } from '@angular/router';

import { SmartContractVersion } from '../shared/smart-contracts.model';
import * as Web3EthAbi from 'web3-eth-abi';
import * as Web3Utils from 'web3-utils';

import { EthApiService } from '../../shared/eth-api.service';
import { HighlightService } from '../../shared/highlight.service';
import { isHexAddress } from '../shared/custom-validators';
import { ContractPayloadPreviewFormComponent } from '../contract-payload-preview-form/contract-payload-preview-form.component';
import { TourService } from '../../shared/tour.service';

const FALSE_HEX_VALUE = '0x0000000000000000000000000000000000000000000000000000000000000000';
const TRUE_HEX_VALUE = '0x0000000000000000000000000000000000000000000000000000000000000001';
const VOID_HEX_VALUE = '0x';

@Component({
  selector: 'athena-smart-contract-version',
  templateUrl: './smart-contract-version.component.html',
  styleUrls: ['./smart-contract-version.component.scss']
})
export class SmartContractVersionComponent implements OnChanges, OnInit {

  @Input() version: SmartContractVersion;
  @ViewChild('payloadPreviewModal') payloadPreviewModal: ContractPayloadPreviewFormComponent;
  functions;
  versionForm: FormGroup;
  inputs = [];
  alertMessage: string;
  alertType: string;
  resultType: string;
  highlightedMetaData: string;
  highlightedSourceCode: string;
  callReturnValue: string;
  functionDefinition;

  constructor(
    private ethApiService: EthApiService,
    private highlighter: HighlightService,
    private translate: TranslateService,
    private route: ActivatedRoute,
    private tourService: TourService,
  ) {
    this.versionForm = new FormGroup({
      functionName: new FormControl(''),
      contractForm: new FormGroup({
        from: new FormControl('', [Validators.required, isHexAddress]),
        functionInputs: new FormGroup({})
      })
    });
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'new':
          this.tourService.startContractTour();
          break;
        default:
          // code...
          break;
      }
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
        this.alertMessage = this.translate.instant('smartContracts.form.callSuccessMessage');
        this.alertType = 'alert-success';
        this.resultType = 'call';

        switch (resp.result) {
          case FALSE_HEX_VALUE:
            this.callReturnValue = 'false';
            break;
          case TRUE_HEX_VALUE:
            this.callReturnValue = 'true';
            break;
          case VOID_HEX_VALUE:
            this.callReturnValue = this.translate.instant('smartContracts.version.emptyResponse');
            break;
          default:
            this.callReturnValue = Web3Utils.hexToAscii(resp.result);
            break;
        }
      }
    }, errorResp => this.handleError(errorResp));
  }

  onSend() {
    this.ethApiService.sendTransaction(this.encodeFunction())
    .subscribe((resp) => {
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
    const bytesRegex = /^byte[s]?\d{0,2}$/;
    const bytesArrayRegex = /^byte[s]?\d{0,2}\[\d*]$/;
    const paramsForm = this.versionForm.get('contractForm').get('functionInputs');
    const params = this.inputs.map((input) => {
      let value = paramsForm.value[input.name];

      if (bytesRegex.test(input.type)) {
        value = Web3Utils.asciiToHex(value);
      } else if (bytesArrayRegex.test(input.type)) {
        value = value.split('\n');
        value = value.map((x) => {
          return Web3Utils.isHexStrict(x) ? x : Web3Utils.asciiToHex(x);
        });
      }

      return value;
    });
    const output = Web3EthAbi.encodeFunctionCall(this.functionDefinition, params);

    return {
      from: this.versionForm.value.contractForm.from,
      to: this.version.address,
      gas: '0xF4240',
      data: output
    };
  }

  private generateFileName() {
    return `${this.version.contract_id}_${this.version.version}`;
  }

  private handleError(error) {
    this.alertMessage = error.error.message ? error.error.message : error.error;
    this.alertType = 'alert-danger';
    this.resultType = 'error';
  }

  private highlightCode() {
    this.highlightedSourceCode = this.highlighter.highlight(this.version.sourcecode, this.highlighter.languages.solidity);
    this.highlightedMetaData = this.highlighter.highlight(JSON.stringify(this.version.metadata, null, 4), this.highlighter.languages.json);
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
    if(Object.keys(version.currentValue.metadata).length === 0) {
      return;
    }
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
    this.highlightCode();
  }
}
