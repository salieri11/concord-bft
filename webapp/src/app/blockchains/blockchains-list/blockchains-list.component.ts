/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, OnInit, Output, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { GridComponent } from '../../grid/grid.component';
import { GridOptions } from '../../grid/shared/grid.model';
import { BlockchainsService } from '../shared/blockchains.service';
import { Blockchain } from '../shared/blockchains.model';
import { Org } from '../../orgs/shared/org.model';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-blockchains-list',
  templateUrl: './blockchains-list.component.html',
  styleUrls: ['./blockchains-list.component.scss']
})
export class BlockchainsListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('grid') grid: GridComponent;
  @Output('selected') selected: EventEmitter<any> = new EventEmitter<any>();

  gridOptions: GridOptions = new GridOptions();

  constructor ( private translate: TranslateService,
                private blockchainsService: BlockchainsService
  ) {
    this.gridOptions.getData = (params?: any) => {
      return this.blockchainsService.getList(params);
    };

    this.translate.get('blockchains.grid')
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
      type: 'link',
      genLink: (row: Blockchain) => {
        return `/blockchains/${row.id}`;
      }
    }, {
      id: 'consensusType',
      name: grid.columns.consensusType.title,
      type: 'string'
    }, {
      id: 'state',
      name: grid.columns.status.title,
      type: 'string'
    }, {
      id: 'lastAction',
      name: grid.columns.action.title,
      type: 'string'
    }, {
      id: 'k8sDashboardUrl',
      name: grid.columns.k8sDashboardUrl.title,
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.k8sDashboardUrl;
      }
    }, {
      id: 'fabricExplorerUrl',
      name: grid.columns.fabricExplorerUrl.title,
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.fabricExplorerUrl;
      }
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }
    ];
  }

}
