/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject } from '@angular/core';

import { CONCORD_API_PREFIX } from './shared.config';
import { BaseApi } from '../base-api';

export abstract class ConcordApiService extends BaseApi {

  constructor(@Inject(CONCORD_API_PREFIX) private concordApiPrefix: string) {
    super();
  }

  get apiPath() {
    return this.concordApiPrefix;
  }
}
