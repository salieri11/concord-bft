/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Personas } from '../../shared/persona.service';

export interface User {
  user_id?: string;
  name?: string;
  email: string;
  password?: string;
  organization?: any;
  consortium?: any;
  consortium_id?: string;
  persona?: Personas;
  createdOn?: number;
  updatedOn?: number;
  last_login?: number;
  role?: Personas;
  token?: string;
  wallet_address?: string;
  details?: {
    first_name: string,
    last_name: string
  };
}

export interface UserAuthResponse {
   auth_token: string;
   last_login: number;
   email: string;
}

export const mockUserDefault: User = {
  role: Personas.SystemsAdmin,
  user_id: '111-111-111', last_login: 1534534154662,
  consortium: { consortium_id: 2, consortium_name: 'TEST_CON' },
  organization: { organization_id: 1, organization_name: 'TEST_ORG' },
  name: 'Test User', details: { last_name: 'User', first_name: 'Test' },
  email: 'test.user@vmware.com'
};

export const mockUserDefault2: User = {
  role: Personas.SystemsAdmin,
  user_id: '222-222-222', last_login: 1534534154662,
  consortium: { consortium_id: 2, consortium_name: 'TEST_CON' },
  organization: { organization_id: 1, organization_name: 'TEST_ORG' },
  name: 'Test User', details: { last_name: 'User', first_name: 'Test' },
  email: 'test.user.2@vmware.com'
};

export const mockUsers = [
  mockUserDefault,
  mockUserDefault2
];
