/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ChangeDetectorRef, Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { EthApiService } from '../../shared/eth-api.service';
import 'rxjs/add/operator/mergeMap';
import { AthenaApiService } from '../../shared/athena-api.service';

import { ADDRESS_LENGTH, ADDRESS_PATTERN } from '../../shared/shared.config';

const addressValidators = [
  Validators.maxLength(ADDRESS_LENGTH),
  Validators.minLength(ADDRESS_LENGTH),
  Validators.pattern(ADDRESS_PATTERN)
];

@Component({
  selector: 'app-testing-ground',
  templateUrl: './testing-ground.component.html',
  styleUrls: ['./testing-ground.component.scss']
})
export class TestingGroundComponent implements OnInit {

  private dataForm: FormGroup;
  private smartContractForm: FormGroup;

  @ViewChild('dataHashRef')
  private dataHashRef: ElementRef;

  @ViewChild('smartContractHashRef')
  private smartContractHashRef: ElementRef;

  private dataHash: string = undefined;
  private smartContractHash: string = undefined;

  constructor(private ethApiService: EthApiService,
              private athenaApiService: AthenaApiService,
              private formBuilder: FormBuilder,
              private changeDetectorRef: ChangeDetectorRef) {

    this.dataForm = this.formBuilder.group({
      from: ['', [Validators.required, ...addressValidators]],
      to:   ['', [Validators.required, ...addressValidators]],
      text: ['', [Validators.required, ...addressValidators]],
    });

    this.dataForm.valueChanges.subscribe(() => this.dataHash = undefined);

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
    this.ethApiService.sendTransaction({
      from: this.dataForm.value.from,
      to: this.dataForm.value.to,
      data: this.dataForm.value.text,
    }).subscribe(result => {
      this.dataHash = result.result;
    });
  }

  onSubmitSmartContract() {
    this.ethApiService.sendTransaction({
      from: this.smartContractForm.value.from,
      data: this.smartContractForm.value.file
    }).flatMap(result => {
      return this.ethApiService.getTransactionReceipt(result.result);
    }).subscribe(result => {
      this.smartContractHash = result.result.contractAddress;
    });
  }

  onCopyDataHash() {
    copyElementToClipboard(this.dataHashRef.nativeElement);
  }

  onCopySmartContractHash() {
    copyElementToClipboard(this.smartContractHashRef.nativeElement);
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

function generateRandomHash() {
  const sampleSpace = '0123456789abcdefgh';
  const hash = [];
  for (let i = 0; i < 64; i++) {
    const pos = Math.floor(Math.random() * sampleSpace.length);
    hash.push(sampleSpace.charAt(pos));
  }
  return hash.join('');
}
