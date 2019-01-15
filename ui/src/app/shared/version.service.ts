/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

export interface Version {
  version: string;
  commit: string;
}

@Injectable({
  providedIn: 'root'
})
export class VersionService {

  constructor(private http: HttpClient) { }

  getCurrent(): Observable<Version> {
    return this.http.get<Version>('./static/data/version.json');
  }
}
