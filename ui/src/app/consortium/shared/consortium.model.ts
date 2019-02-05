/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface Consortium {
  id?: number;
  name: string;
  members: Array<string>;

  createdOn?: number;
  updatedOn?: number;
}

export interface ConsortiumResponse {
  _embedded: {
    organizations: Array<Consortium>,
    _links: any,
  };
  page: {
    size: number,
    totalElements: number,
    totalPages: number
  };
}
