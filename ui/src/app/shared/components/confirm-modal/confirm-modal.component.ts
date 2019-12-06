/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'concord-confirm-modal',
  templateUrl: './confirm-modal.component.html',
  styleUrls: ['./confirm-modal.component.scss']
})
export class ConfirmModalComponent implements OnInit {
  component: any;
  action: string;
  message: string;
  confirmBtn: string;
  open: boolean;

  constructor() { }

  ngOnInit() {
  }

  openModal(component: any, action: string, message: string, confirmBtn: string) {
    this.component = component;
    this.action = action;
    this.message = message;
    this.confirmBtn = confirmBtn;
    this.open = true;
  }

  confirmed(): any {
    return this.component[this.action]();
  }
}
