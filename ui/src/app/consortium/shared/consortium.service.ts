/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { ConsortiumResponse } from './consortium.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class ConsortiumService {

  constructor(
    private http: HttpClient,
  ) {}

  getList(): Observable<ConsortiumResponse[]> {
    return this.http.get<ConsortiumResponse[]>(Apis.consortiums);
  }

  create(name: string): Observable<ConsortiumResponse> {
    return this.http.post<ConsortiumResponse>(Apis.consortiums, {consortium_name: name});
  }

}
