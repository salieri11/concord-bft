/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
  Input,
  Output,
  EventEmitter,
  NgZone,
} from '@angular/core';
import {
  ClrDatagrid,
  ClrDatagridStateInterface,
  ClrDatagridPagination
} from '@clr/angular';


import {
  GridOptions,
  GridListResponse,
  GridMeta
} from './shared/grid.model';
import { ErrorAlertService } from '../shared/global-error-handler.service';

@Component({
  selector: 'app-grid',
  templateUrl: './grid.component.html',
  styleUrls: ['./grid.component.scss']
})
export class GridComponent implements OnInit {
  @Input('options') options: GridOptions;
  @Output() selectedRowsEvent: EventEmitter<any> = new EventEmitter<any>();
  @ViewChild('datagrid') datagrid: ClrDatagrid;
  @ViewChild('pagination') pagination: ClrDatagridPagination;

  loading = false;
  rows: Array<any> = [];
  meta: GridMeta = new GridMeta();
  selectedRows: Array<any> = [];
  search = '';
  selectedSize = 10;
  previousState: ClrDatagridStateInterface;

  constructor(
    private errorService: ErrorAlertService,
    private zone: NgZone,
  ) {}

  ngOnInit() {
    this.datagrid.selection.change
      .subscribe(
        selectionChange => this.handleRowSelection(selectionChange)
      );
  }

  refresh(state?: ClrDatagridStateInterface): void {
    setTimeout(() => {
      this.previousState = state;
      this.loading = true;

      const filters: { [prop: string]: any } = {};
      if (state && state.filters) {
        for (const filter of state.filters) {
          const { property, value } = <{ property: string, value: string }>filter;
          filters[property] = [value];
        }
      }

      filters['size'] = this.selectedSize;

      if (state && state.page && state.page.size) {
        filters['page'] = this.getPage(state);
      }

      if (state && state.sort) {
        const direction = state.sort.reverse ? 'asc' : 'desc';
        filters['sort'] = `${state.sort.by},${direction}`;
      }

      this.options.getData(filters)
        .subscribe(
          rows => this.handleResponse(rows),
          error => this.handleError(error)
        );
    }, 10);
  }

  searchList(): void {
    this.options.getData({
      search: this.search, size: this.selectedSize
    })
      .debounceTime(500)
      .subscribe(
        rows => this.handleResponse(rows),
        error => this.handleError(error)
      );
  }

  addRow(row: any): void {
    row._flash = true;
    this.zone.run(() => {
      this.meta.total = ++this.meta.total;
    });
    this.rows.unshift(row);
  }

  reload(): void {
    this.clearSelectedRows();
    this.refresh(this.previousState);
  }

  clearSelection(): void {
    this.clearSelectedRows();
  }

  private getPage(state: ClrDatagridStateInterface): number {
    const size = state.page.size,
      to = state.page.to + 1,
      page = Math.ceil(to / size) - 1;

    return page;
  }

  private clearSelectedRows(): void {
    this.selectedRows = [];
  }

  private handleRowSelection(change: Array<any>): void {
    setTimeout(() => {
      this.selectedRowsEvent.emit(change);
    }, 10);
  }

  private handleResponse(response: GridListResponse): void {
    this.meta = response.meta;
    this.pagination.totalItems = this.meta.total;
    this.rows = response.objects;
    this.loading = false;
  }

  private handleError(error): void {
    this.loading = false;
    this.errorService.add(error);
  }
}
