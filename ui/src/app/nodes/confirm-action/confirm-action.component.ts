/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, Output, EventEmitter } from '@angular/core';

import { Node } from './../shared/nodes.model';

@Component({
  selector: 'concord-confirm-action',
  templateUrl: './confirm-action.component.html',
  styleUrls: ['./confirm-action.component.scss']
})
export class ConfirmActionComponent implements OnInit {
  opened: boolean;
  node: Node | Node[];
  action: string;
  confrimation: string;
  expectedConfirmation: string = 'NODE STOP';
  @Output('confirmed') confirmed: EventEmitter<any> = new EventEmitter<any>();

  constructor() { }

  ngOnInit() {
  }

  open(action: string, node: Node | Node[]) {
    this.action = action;
    this.node = node;
    this.opened = true;
  }

  close() {
    this.opened = false;
  }

  confirm() {
    this.close();
    this.confirmed.emit({action: this.action, node: this.node});
  }

}
