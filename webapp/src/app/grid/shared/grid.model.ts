/*
 * Copyright 2018 VMware, all rights reserved.
 */

export interface GridRenderCell {
  main: Function;
  info: Function;
}

export interface GridColumn {
  id: string;
  name: string;
  type: string;
  hidden?: boolean;
  renderCell?: GridRenderCell;
  genLink?: Function;
}

export class GridOptions {
  getData: any;
  url?: string;
  columns: Array<GridColumn>;
  paginationTitle: string;
  constructor() {}
}

export class GridMeta {
  size: number;
  total: number;
  totalPages: number;
  constructor() {}
}

export interface GridListResponse {
  objects: Array<any>;
  meta: GridMeta;
}
