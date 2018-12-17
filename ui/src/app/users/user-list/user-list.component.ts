/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';

import { Personas } from '../../shared/persona.service';
import { User } from '../shared/user.model';

@Component({
  selector: 'concord-user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin, Personas.OrgAdmin];
  @Input('users') users: User[];
  @Output('selected') selected: EventEmitter<any> = new EventEmitter<any>();
  @ViewChild('datagrid') datagrid: ClrDatagrid;

  selectedUsers: User[] = [];

  constructor() {

  }

  ngOnInit() {
    this.datagrid.selection.change.subscribe(
      selectionChange => this.handleRowSelection(selectionChange)
    );
  }

  private handleRowSelection(change: Array<any>): void {
    setTimeout(() => {
      this.selected.emit(change);
    }, 10);
  }
}
