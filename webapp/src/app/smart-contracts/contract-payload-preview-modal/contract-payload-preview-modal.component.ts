/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

@Component({
  selector: 'app-contract-payload-preview-modal',
  templateUrl: './contract-payload-preview-modal.component.html',
  styleUrls: ['./contract-payload-preview-modal.component.scss']
})
export class ContractPayloadPreviewModalComponent {
  isOpen = false;
  payloadPreview: string;

  constructor() {
  }

  open(payloadPreview: string) {
    this.payloadPreview = payloadPreview;
    this.isOpen = true;
  }

  onClose() {
    this.isOpen = false;
  }
}
