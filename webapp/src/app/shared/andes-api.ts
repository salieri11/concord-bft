/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject } from '@angular/core';
import { HttpHeaders } from '@angular/common/http';

import { ANDES_API_PREFIX } from './shared.config';
import { BaseApi } from '../base-api';

export abstract class AndesApi extends BaseApi {
  headers: HttpHeaders = new HttpHeaders({
    'Content-Type':  'application/json'
  });

  constructor (@Inject(ANDES_API_PREFIX) private andesApiPrefix: string) {
    super();
  }

  get apiPath() {
    return this.andesApiPrefix;
  }
}
