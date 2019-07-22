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
