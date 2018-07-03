/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, OnInit, Output, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { Personas } from '../../shared/persona.service';
import { GridComponent } from '../../grid/grid.component';
import { GridOptions } from '../../grid/shared/grid.model';
import { Consortium } from '../shared/consortium.model';
import { ConsortiumService } from '../shared/consortium.service';
import { Org } from '../../orgs/shared/org.model';

@Component({
  selector: 'athena-consortiums-list',
  templateUrl: './consortiums-list.component.html',
  styleUrls: ['./consortiums-list.component.scss']
})
export class ConsortiumsListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('grid') grid: GridComponent;
  @Output() selected: EventEmitter<any> = new EventEmitter<any>();

  gridOptions: GridOptions = new GridOptions();

  constructor(
    private consortiumService: ConsortiumService,
    private translate: TranslateService
  ) {

    this.gridOptions.getData = () => {
      return this.consortiumService.getFakeData();
      // return this.consortiumService.getList(params);
    };

    this.translate.get('consortium.grid')
      .subscribe(grid => this.handleGrid(grid));
  }


  ngOnInit() {

  }

  selectedRowChange(rows: Array<Org>): void {
    this.selected.emit(rows);
  }

  private handleGrid(grid: any): void {
    this.gridOptions.paginationTitle = grid.pagination.title;
    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'string'
    }, {
      id: 'members',
      name: grid.columns.members.title,
      type: 'info',
      renderCell: {
        main: (row: Consortium) => {
          return row.members.length;
        },
        info: (row: Consortium) => {
          return `<h3>Members</h3>
            <ol class="list">
              ${row.members.join(',').split(',').map((members) => `
              <li>${members}</li>`).join('')}
            </ol>`;
        }
      }
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }];
  }

}
