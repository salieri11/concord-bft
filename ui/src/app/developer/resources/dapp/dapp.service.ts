/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class DappService {

  constructor(private http: HttpClient) { }

  deploySupplyChainDapp(): Observable<any> {
    return this.http.get('dapp/api/deploy');
  }

  checkIfDeployed(): Observable<any> {
    return this.http.get('dapp/api/deploy/check');
  }
}
