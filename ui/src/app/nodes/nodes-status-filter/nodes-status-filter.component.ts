/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder } from '@angular/forms';
import { ClrDatagridFilterInterface } from '@clr/angular';
import { Subject } from 'rxjs';

import { NodeInfo } from '../shared/nodes.model';

@Component({
  selector: 'concord-nodes-status-filter',
  templateUrl: './nodes-status-filter.component.html',
  styleUrls: ['./nodes-status-filter.component.scss']
})
export class NodesStatusFilterComponent implements OnInit, ClrDatagridFilterInterface<NodeInfo> {
  readonly form: FormGroup;

  changes = new Subject<any>();

  constructor(private formBuilder: FormBuilder) {
    this.form = this.formBuilder.group({
      filterOption: ['']
    });
  }

  ngOnInit() {
  }

  onOptionChange() {
    this.changes.next(true);
  }
  isActive(): boolean {
    return this.form.controls.filterOption.value !== '';
  }
  accepts(member: NodeInfo): boolean {
    return this.form.controls.filterOption.value === '' || member.status === this.form.controls.filterOption.value;
  }

}
