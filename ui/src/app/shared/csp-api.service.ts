/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { CspRequestTokenResponse } from '../logging/shared/logging.model';
import { CSP_API_PREFIX } from './shared.config';

@Injectable({
  providedIn: 'root'
})
export class CspApiService {

  constructor(@Inject(CSP_API_PREFIX) private cspApiPrefix: string, private httpClient: HttpClient) { }

  fetchToken(): Observable<CspRequestTokenResponse> {
    const body = new HttpParams()
      .set('refresh_token', '01e669b9-9599-48ed-8248-4c749bbff1ff');
    return this.httpClient.post<CspRequestTokenResponse>(this.cspApiPrefix,
      body.toString(),
      {
        headers: new HttpHeaders()
          .set('Content-Type', 'application/x-www-form-urlencoded')
      }
    );
  }
}
