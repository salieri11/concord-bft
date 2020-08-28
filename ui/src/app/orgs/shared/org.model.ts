/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface OrgProperties {
  max_chains: number;
}

export interface Org {
  organization_id: string;
  organization_name: string;
  organization_properties: OrgProperties;
}

export const mockOrgDefault = {
  organization_id: '00000000-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  organization_name: 'MOCK_ORG',
  organization_properties: { max_chains: '0' },
};

export const mockOrgs = [
  mockOrgDefault
];
