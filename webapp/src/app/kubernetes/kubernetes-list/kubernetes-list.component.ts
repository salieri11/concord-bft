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

    this.handleGrid();
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<Org>): void {
    this.selected.emit(rows);
  }

  private handleGrid(): void {
    this.gridOptions.paginationTitle = this.translate.instant('kubernetes.grid.pagination.title');
    this.gridOptions.columns = [{
      id: 'name',
      name: this.translate.instant('kubernetes.grid.columns.name.title'),
      type: 'string'
    }, {
      id: 'apiServerUrl',
      name: this.translate.instant('kubernetes.grid.columns.apiServerUrl.title'),
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return `http://${row.apiServerUrl}`;
      }
    }, {
      id: 'kubectlCliUrl',
      name: this.translate.instant('kubernetes.grid.columns.kubectlCliUrl.title'),
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.kubectlCliUrl;
      }
    }, {
      id: 'helmCliUrl',
      name: this.translate.instant('kubernetes.grid.columns.helmCliUrl.title'),
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.helmCliUrl;
      }
    }, {
      id: 'ca',
      name: this.translate.instant('kubernetes.grid.columns.ca.title'),
      type: 'string'
    }, {
      id: 'credentialType',
      name: this.translate.instant('kubernetes.grid.columns.credentialType.title'),
      type: 'string'
    }, {
      id: 'insecure',
      name: this.translate.instant('kubernetes.grid.columns.insecure.title'),
      type: 'string'
    }, {
      id: 'createdOn',
      name: this.translate.instant('kubernetes.grid.columns.created.title'),
      type: 'date'
    }, {
      id: 'updatedOn',
      name: this.translate.instant('kubernetes.grid.columns.updated.title'),
      type: 'date'
    }];
  }
}
