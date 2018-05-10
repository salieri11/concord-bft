/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ClrDatagridFilterInterface } from '@clr/angular';
import { Subject } from 'rxjs/Subject';

import { Member } from '../../shared/remote-interfaces';

@Component({
  selector: 'app-nodes-status-filter',
  templateUrl: './nodes-status-filter.component.html',
  styleUrls: ['./nodes-status-filter.component.scss']
})
export class NodesStatusFilterComponent implements OnInit, ClrDatagridFilterInterface<Member> {
  options: string[] = [
    '',
    'connected',
    'offline'
  ];
  selectedOption = '';
  changes = new Subject<any>();

  constructor() { }

  ngOnInit() {
  }

  onOptionChange() {
    this.changes.next(true);
  }
  isActive(): boolean {
    return this.selectedOption !== '';
  }
  accepts(member: Member): boolean {
    return this.selectedOption === '' || member.status === this.selectedOption;
  }

}
