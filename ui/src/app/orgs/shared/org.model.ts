/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface OrgProperties {
  max_chains: number;
}

export interface Org {
  id: string;
  organization_name: string;
  organization_properties: OrgProperties;
}
