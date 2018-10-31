/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Personas } from '../../shared/persona.service';

export interface User {
  user_id?: number;
  name?: string;
  email: string;
  password?: string;
  organization?: any;
  consortium?: any;
  persona?: Personas;
  createdOn?: number;
  updatedOn?: number;
  last_login?: number;
  role?: Personas;
  token?: string;
  details?: {
    first_name: string,
    last_name: string
  };
}
