/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject } from '@angular/core';

import { ATHENA_API_PREFIX } from './shared.config';
import { BaseApi } from '../base-api';

export abstract class AthenaApiService extends BaseApi {

  constructor(@Inject(ATHENA_API_PREFIX) private athenaApiPrefix: string) {
    super();
  }

  get apiPath() {
    return this.athenaApiPrefix;
  }
}
