/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ChangeDetectorRef, Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import 'rxjs/add/operator/mergeMap';

import { EthApiService } from '../../shared/eth-api.service';

import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

enum TransactionActionOptions {
  call = 'call',
  transaction = 'transaction'
}

@Component({
  selector: 'app-testing-ground',
  templateUrl: './testing-ground.component.html',
  styleUrls: ['./testing-ground.component.scss']
})
export class TestingGroundComponent implements OnInit {
  transactionActionOptions = TransactionActionOptions;
  dataForm: FormGroup;
  smartContractForm: FormGroup;

  @ViewChild('dataHashRef')
  dataHashRef: ElementRef;

  @ViewChild('smartContractHashRef')
  smartContractHashRef: ElementRef;

  dataHash: string = undefined;
  smartContractHash: string = undefined;

  constructor(private ethApiService: EthApiService,
              private formBuilder: FormBuilder,
              private changeDetectorRef: ChangeDetectorRef) {

    this.dataForm = this.formBuilder.group({
      from:  ['', [Validators.required, ...addressValidators]],
      to:    ['', [Validators.required, ...addressValidators]],
      value: ['', Validators.required],
      text:  ['', [Validators.required, Validators.pattern(ADDRESS_PATTERN)]],
      type:  [TransactionActionOptions.transaction],
    });

    this.dataForm.valueChanges.subscribe(() => this.dataHash = undefined);

    this.dataForm.controls.type.valueChanges.subscribe((changes) => {
      switch (changes) {
        case TransactionActionOptions.transaction:
          this.dataForm.controls.from.setValidators([Validators.required, ...addressValidators]);
          this.dataForm.controls.from.updateValueAndValidity({emitEvent : false});
          break;
        case TransactionActionOptions.call:
          this.dataForm.controls.from.clearValidators();
          this.dataForm.controls.from.updateValueAndValidity({emitEvent : false});
          break;
      }
    });

    this.dataForm.controls.text.valueChanges.subscribe((changes) => {
      if (changes.length > 0) {
        // Value is no longer required
        this.dataForm.controls.value.clearValidators();
        this.dataForm.controls.text.setValidators([Validators.required, Validators.pattern(ADDRESS_PATTERN)]);
        this.dataForm.controls.value.updateValueAndValidity({emitEvent : false});
        this.dataForm.controls.text.updateValueAndValidity({emitEvent : false});
      }
    });

    this.dataForm.controls.value.valueChanges.subscribe((changes) => {
      if (changes.length > 0) {
        // Text is no longer required
        this.dataForm.controls.text.clearValidators();
        this.dataForm.controls.value.setValidators([Validators.required, Validators.pattern(ADDRESS_PATTERN)]);
        this.dataForm.controls.text.updateValueAndValidity({emitEvent : false});
        this.dataForm.controls.value.updateValueAndValidity({emitEvent : false});
      }
    });

    this.smartContractForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      file: [null, Validators.required],
    });
    this.smartContractForm.valueChanges.subscribe(() => this.smartContractHash = undefined);
  }

  ngOnInit() {
  }

  onSmartContractFileChange(event) {
    if (event.target.files.length === 0) {
      this.smartContractForm.patchValue({
        file: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.smartContractForm.patchValue({
        file: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  onSubmitData() {
    switch (this.dataForm.value.type) {
      case TransactionActionOptions.transaction:
        this.submitTransaction();
        break;
      case TransactionActionOptions.call:
        this.submitCall();
        break;
    }
  }

  submitTransaction() {
    this.ethApiService.sendTransaction({
      from: this.dataForm.value.from,
      to: this.dataForm.value.to,
      data: this.dataForm.value.text.length === 0 ? null : this.dataForm.value.text,
      value: this.dataForm.value.value.length === 0 ? null : this.dataForm.value.value,
    }).subscribe(response => {
      this.dataHash = response.result;
    }, response => {
      alert(response.error);
    });
  }

  submitCall() {
    this.ethApiService.sendCall({
      from: this.dataForm.value.from.length === 0 ? null : this.dataForm.value.from,
      to: this.dataForm.value.to,
      data: this.dataForm.value.text.length === 0 ? null : this.dataForm.value.text,
      value: this.dataForm.value.value.length === 0 ? null : this.dataForm.value.value,
    }).subscribe(response => {
      this.dataHash = response.result;
    }, response => {
      alert(response.error);
    });
  }

  onSubmitSmartContract() {
    this.ethApiService.sendTransaction({
      from: this.smartContractForm.value.from,
      data: this.smartContractForm.value.file
    }).flatMap(response => {
      return this.ethApiService.getTransactionReceipt(response.result);
    }).subscribe(response => {
      this.smartContractHash = response.result.contractAddress;
    }, response => {
      alert(response.error);
    });
  }

  onCopyDataHash() {
    copyElementToClipboard(this.dataHashRef.nativeElement);
  }

  onCopySmartContractHash() {
    copyElementToClipboard(this.smartContractHashRef.nativeElement);
  }

  get isTransaction() {
    return this.dataForm.value.type === TransactionActionOptions.transaction;
  }
}

function copyElementToClipboard(element) {
  const range = document.createRange();
  range.selectNode(element);
  window.getSelection().removeAllRanges();
  window.getSelection().addRange(range);

  try {
    if (!document.execCommand('copy')) {
      alert('Your browser doesn\'t support this feature');
    }
  } catch (err) {
    alert('Your browser doesn\'t support this feature');
  }

  window.getSelection().removeAllRanges();
}
