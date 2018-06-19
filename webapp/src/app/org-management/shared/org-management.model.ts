/*
 * Copyright 2018 VMware, all rights reserved.
 */

export interface Org {
  id?: number;
  name: string;
  domain: string;
  peerNumber: number;
  type: string;
  createdOn?: number;
}

export interface OrgResponse {
  _embedded: {
    organizations: Array<Org>,
    _links: any,
  };
  page: {
    size: number,
    totalElements: number,
    totalPages: number
  };
}
