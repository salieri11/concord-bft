/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { Observable } from 'rxjs';
import { Node } from './nodes.model';
import { ATHENA_API_PREFIX } from '../../shared/shared.config';
import { AthenaApiService } from '../../shared/athena-api';

@Injectable({
  providedIn: 'root'
})
export class NodesService extends AthenaApiService {

  constructor(@Inject(ATHENA_API_PREFIX) athenaApiPrefix: string, private httpClient: HttpClient) {
    super(athenaApiPrefix);
  }

  get apiSubPath() {
    return 'members';
  }

  getNodes(): Observable<Node[]> {
    return this.httpClient.get<Node[]>(this.resourcePath());
  }
}
