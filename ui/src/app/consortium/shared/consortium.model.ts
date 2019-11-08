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
  organization_id: string;
  consortium_id: string;
  consortium_name: string;
}
