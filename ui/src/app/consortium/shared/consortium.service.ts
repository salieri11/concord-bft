/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { ConsortiumResponse } from './consortium.model';

@Injectable({
  providedIn: 'root'
})
export class ConsortiumService {
  apiPath = 'api/consortiums';

  constructor(
    private http: HttpClient,
  ) {}

  getList(): Observable<ConsortiumResponse[]> {
    return this.http.get<ConsortiumResponse[]>(this.apiPath);
  }

  create(name: string): Observable<ConsortiumResponse> {
    return this.http.post<ConsortiumResponse>(this.apiPath, {consortium_name: name});
  }

}
