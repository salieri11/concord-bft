/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject } from '@angular/core';

import { ATHENA_API_PREFIX } from './shared.config';

export class AthenaApiService {

  constructor(
    @Inject(ATHENA_API_PREFIX) private athenaApiPrefix: string,
  ) {}

  apiPath(path: string) {
    return `${this.athenaApiPrefix}${path}`;
  }
}
