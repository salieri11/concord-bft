/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Input,
  Component,
  OnInit,
  ViewChild, Output, EventEmitter,
} from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { GridOptions } from '../../grid/shared/grid.model';
import { GridComponent } from '../../grid/grid.component';
import { OrgService } from '../shared/org.service';
import { Org } from '../shared/org.model';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'concord-org-list',
  templateUrl: './org-list.component.html',
  styleUrls: ['./org-list.component.scss']
})
export class OrgListComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('grid') grid: GridComponent;
  @Input('url') url: string;
  @Output('selected') selected: EventEmitter<any> = new EventEmitter<any>();

  gridOptions: GridOptions = new GridOptions();

  constructor(
    private orgService: OrgService,
    private translate: TranslateService,
  ) {
    const browserLang = translate.getBrowserLang();
    this.translate.setDefaultLang('en');
    this.translate.use(browserLang);

    this.gridOptions.getData = (params: any) => {
      return this.orgService.getList(params, this.url);
    };

    this.handleGrid();
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<Org>): void {
    this.selected.emit(rows);
  }

  private handleGrid(): void {
    this.gridOptions.paginationTitle = this.translate.instant('organization.grid.pagination.title');

    this.gridOptions.columns = [{
      id: 'name',
      name: this.translate.instant('organization.grid.columns.name.title'),
      type: 'string'
    }, {
      id: 'domain',
      name: this.translate.instant('organization.grid.columns.domain.title'),
      type: 'string'
    }, {
      id: 'type',
      name: this.translate.instant('organization.grid.columns.type.title'),
      type: 'string'
    }, {
      id: 'createdOn',
      name: this.translate.instant('organization.grid.columns.created.title'),
      type: 'date'
    }];
  }
}
