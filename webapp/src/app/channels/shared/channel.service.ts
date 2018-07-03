/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';

import { AndesApi } from '../../shared/andes-api';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

@Injectable({
  providedIn: 'root'
})
export class ChannelService extends AndesApi {

  constructor(@Inject(ANDES_API_PREFIX) andesApiPrefix: string) {
    super(andesApiPrefix);
  }

  get apiSubPath() {
    return ''; // TODO: replace this with the channels API path
  }

}
