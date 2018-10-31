/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

import { HighlightService } from '../../shared/highlight.service';

@Component({
  selector: 'athena-contract-payload-preview-form',
  templateUrl: './contract-payload-preview-form.component.html',
  styleUrls: ['./contract-payload-preview-form.component.scss']
})
export class ContractPayloadPreviewFormComponent {
  isOpen = false;
  payloadPreview: string;

  constructor(private highlighter: HighlightService) {
  }

  open(payloadPreview: string) {
    this.payloadPreview = this.highlighter.highlight(payloadPreview, this.highlighter.languages.json);
    this.isOpen = true;
  }

  onClose() {
    this.isOpen = false;
  }
}
