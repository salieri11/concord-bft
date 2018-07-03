/*
 * Copyright 2018 VMware, all rights reserved.
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
  selector: 'athena-org-list',
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

    translate.get('organization.grid')
      .subscribe(grid => this.handleGrid(grid));
  }

  ngOnInit() {
  }

  selectedRowChange(rows: Array<Org>): void {
    this.selected.emit(rows);
  }

  private handleGrid(grid: any): void {
    console.log('grid');
    console.log(grid);
    this.gridOptions.paginationTitle = grid.pagination.title;

    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'string'
    }, {
      id: 'domain',
      name: grid.columns.domain.title,
      type: 'string'
    }, {
      id: 'type',
      name: grid.columns.type.title,
      type: 'string'
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }];
  }
}
