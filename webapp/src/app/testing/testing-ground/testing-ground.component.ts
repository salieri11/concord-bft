/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';

@Component({
  selector: 'app-testing-ground',
  templateUrl: './testing-ground.component.html',
  styleUrls: ['./testing-ground.component.css']
})
export class TestingGroundComponent implements OnInit {

  @ViewChild('dataHashRef')
  private dataHashRef: ElementRef;

  @ViewChild('smartContractHashRef')
  private smartContractHashRef: ElementRef;

  private dataText = '';
  private smartContractFile = undefined;

  private dataHash: string = undefined;
  private smartContractHash: string = undefined;

  constructor() { }

  ngOnInit() {
  }

  onDataTextChange() {
    this.dataHash = undefined;
  }

  onSmartContractFileChange(files) {
    this.smartContractFile = files[0];
    this.smartContractHash = undefined;
  }

  onSubmitData() {
    console.log(this.dataText);
    this.dataHash = generateRandomHash();
  }

  onSubmitSmartContract() {
    const reader = new FileReader();
    reader.onload = function() {
      console.log(reader.result);
    };
    reader.readAsText(this.smartContractFile);
    this.smartContractHash = generateRandomHash();
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
