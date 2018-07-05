/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, OnInit, Output, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { Personas } from '../../shared/persona.service';
import { GridOptions } from '../../grid/shared/grid.model';
import { UsersService } from '../shared/users.service';
import { GridComponent } from '../../grid/grid.component';
import { User } from '../shared/user.model';

@Component({
  selector: 'athena-user-list',
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('grid') grid: GridComponent;
  @Output('selected') selected: EventEmitter<any> = new EventEmitter<any>();

  gridOptions: GridOptions = new GridOptions();

  constructor(private translate: TranslateService, private usersService: UsersService) {
    this.gridOptions.getData = () => {
      return this.usersService.getFakeData();
    };

    this.handleGrid();
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<User>): void {
    this.selected.emit(rows);
  }

  handleGrid(): void {
    this.gridOptions.paginationTitle = this.translate.instant('users.grid.pagination.title');
    this.gridOptions.columns = [{
      id: 'name',
      name: this.translate.instant('users.grid.columns.name.title'),
      type: 'string'
    }, {
      id: 'email',
      name: this.translate.instant('users.grid.columns.email.title'),
      type: 'string'
    }, {
      id: 'organization',
      name: this.translate.instant('users.grid.columns.organization.title'),
      type: 'string'
    }, {
      id: 'persona',
      name: this.translate.instant('users.grid.columns.persona.title'),
      type: 'string'
    }, {
      id: 'createdOn',
      name: this.translate.instant('users.grid.columns.createdOn.title'),
      type: 'date'
    }, {
      id: 'updatedOn',
      name: this.translate.instant('users.grid.columns.updatedOn.title'),
      type: 'date'
    }
    ];
  }
}
