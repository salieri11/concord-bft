/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { HttpParams } from '@angular/common/http';

const EMPTY_VALUES: Array<string | number> = [undefined, null, ''];

export abstract class BaseApi {

  constructor () {}

  abstract get apiPath(): string;

  abstract get apiSubPath(): string;

  buildHttpParams(params: any) {
    let httpParams = new HttpParams();

    for (const prop in params) {
      if (params[prop]) {
        httpParams = httpParams.set(prop, params[prop]);
      }
    }

    return httpParams;
  }

  resourcePath(path: string | number = '') {
    return [this.apiPath, this.apiSubPath, path].filter(segment => EMPTY_VALUES.indexOf(segment) === -1).join('/');
  }
}
