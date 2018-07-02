/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Personas } from '../../../shared/persona.service';

export interface User {
  id?: number;
  firstName?: string;
  lastName?: string;
  email: string;
  persona: Personas;
  createdOn?: number;
  updatedOn?: number;
}
