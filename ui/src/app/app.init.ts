/*
 * Copyright 2019 VMware, all rights reserved.
 */


import { Injectable } from '@angular/core';
import { from } from 'rxjs';
import { map } from 'rxjs/operators';
import { FeatureFlagService } from './shared/feature-flag.service';
declare var window: any;

@Injectable()
export class AppInitService {

  constructor(
    private featureFlagService: FeatureFlagService
  ) {}

  // This is the method you want to call at bootstrap
  // Important: It should return a Promise
  public init() {
    return from(
        fetch('static/app-config.json').then( (response) => {
          return response.json();
        })
      ).pipe(
        map((config) => {
          window.config = config;
        })
    ).toPromise();
  }
}
