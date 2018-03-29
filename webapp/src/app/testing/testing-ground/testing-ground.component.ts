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

  @ViewChild('textHashElement')
  private textHashChild: ElementRef;

  @ViewChild('fileHashElement')
  private fileHashChild: ElementRef;

  private userText = '';
  private userFile = undefined;

  private textHash: string = undefined;
  private fileHash: string = undefined;

  constructor() { }

  ngOnInit() {
  }

  onTextChange() {
    this.textHash = undefined;
  }

  onFileChange(files) {
    this.userFile = files[0];
    this.fileHash = undefined;
  }

  onSubmitText() {
    console.log(this.userText);
    this.textHash = generateRandomHash();
  }

  onSubmitFile() {
    const reader = new FileReader();
    reader.onload = function() {
      console.log(reader.result);
    };
    reader.readAsText(this.userFile);
    this.fileHash = generateRandomHash();
  }

  onCopyTextHash() {
    copyElementToClipboard(this.textHashChild.nativeElement);
  }

  onCopyFileHash() {
    copyElementToClipboard(this.fileHashChild.nativeElement);
  }
}

function copyElementToClipboard(element) {
  const range = document.createRange();
  range.selectNode(element);
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
