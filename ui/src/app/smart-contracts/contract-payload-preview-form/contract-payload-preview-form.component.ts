/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

@Component({
  selector: 'concord-contract-payload-preview-form',
  templateUrl: './contract-payload-preview-form.component.html',
  styleUrls: ['./contract-payload-preview-form.component.scss']
})
export class ContractPayloadPreviewFormComponent {
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
    this.payloadPreview = null;
  }
}
