/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, OnInit, Output, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { Personas } from '../../shared/persona.service';
import { GridComponent } from '../../grid/grid.component';
import { GridOptions } from '../../grid/shared/grid.model';
import { KubernetesService } from '../shared/kubernetes.service';
import { Kubernetes } from '../shared/kubernetes.model';
import { Org } from '../../orgs/shared/org.model';

@Component({
  selector: 'athena-kubernetes-list',
  templateUrl: './kubernetes-list.component.html',
  styleUrls: ['./kubernetes-list.component.scss']
})
export class KubernetesListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('grid') grid: GridComponent;
  @Output('selected') selected: EventEmitter<any> = new EventEmitter<any>();

  gridOptions: GridOptions = new GridOptions();

  constructor(
    private translate: TranslateService,
    private kubeService: KubernetesService
  ) {
    this.gridOptions.getData = (params?: any) => {
      return this.kubeService.getList(params);
    };

    this.translate.get('kubernetes.grid')
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
      id: 'apiServerUrl',
      name: grid.columns.apiServerUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return `http://${row.apiServerUrl}`;
      }
    }, {
      id: 'kubectlCliUrl',
      name: grid.columns.kubectlCliUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.kubectlCliUrl;
      }
    }, {
      id: 'helmCliUrl',
      name: grid.columns.helmCliUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.helmCliUrl;
      }
    }, {
      id: 'ca',
      name: grid.columns.ca.title,
      type: 'string'
    }, {
      id: 'credentialType',
      name: grid.columns.credentialType.title,
      type: 'string'
    }, {
      id: 'insecure',
      name: grid.columns.insecure.title,
      type: 'string'
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }, {
      id: 'updatedOn',
      name: grid.columns.updated.title,
      type: 'date'
    }];
  }
}
