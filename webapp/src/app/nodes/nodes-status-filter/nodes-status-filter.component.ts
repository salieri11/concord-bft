/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder } from '@angular/forms';
import { ClrDatagridFilterInterface } from '@clr/angular';
import { Subject } from 'rxjs/Subject';

import { Member } from '../../shared/remote-interfaces';

@Component({
  selector: 'app-nodes-status-filter',
  templateUrl: './nodes-status-filter.component.html',
  styleUrls: ['./nodes-status-filter.component.scss']
})
export class NodesStatusFilterComponent implements OnInit, ClrDatagridFilterInterface<Member> {
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
  accepts(member: Member): boolean {
    return this.form.controls.filterOption.value === '' || member.status === this.form.controls.filterOption.value;
  }

}
