/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Personas } from '../../shared/persona.service';

export interface User {
  id?: number;
  firstName?: string;
  lastName?: string;
  email: string;
  persona: Personas;
  createdOn?: number;
  updatedOn?: number;
}


export interface UserResponse {
  _embedded: {
    users: Array<User>,
    _links: any,
  };
  page: {
    size: number,
    totalElements: number,
    totalPages: number
  };
}
