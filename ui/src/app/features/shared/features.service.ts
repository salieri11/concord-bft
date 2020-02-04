/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map } from 'rxjs/operators';

import { Feature } from './feature.model';
import { Apis } from '../../shared/urls.model';

@Injectable({
  providedIn: 'root'
})
export class FeaturesService {

  constructor(
    private http: HttpClient
  ) { }

  getList() {
    return this.http.get<Feature[]>(Apis.features).pipe(
      map(features => {

        return {
          features: features
        };
      })
    );
  }
}
