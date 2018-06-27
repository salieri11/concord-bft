/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { Member } from './nodes.model';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { AthenaApiService } from '../../shared/athena-api.service';

const NODES_PATH = '/members/';

@Injectable({
  providedIn: 'root'
})
export class NodesService extends AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) athenaApiPrefix: string, private httpClient: HttpClient) {
    super(athenaApiPrefix);
  }

  getMembers() {
    return this.httpClient.get<Member[]>(this.apiPath(NODES_PATH));
  }

}
