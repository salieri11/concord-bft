/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { ActivatedRouteSnapshot } from '@angular/router';
import { FeatureFlagSource, FeatureFlagRouteMapping } from './urls.model';

@Injectable({
  providedIn: 'root'
})
export class FeatureFlagService {
  private featureFlags: {}; // Key-value map from JSON file
  private routeFeatureMapping = FeatureFlagRouteMapping;

  constructor(private http: HttpClient) {}

  /**
   * Once initial feature flags data is obtained, import them. \
   * (Only in non-production (UI dev env), import overrides from app-config)
   */
  initialize(): Observable<{}> {
    if (this.featureFlags) {
      return of(this.featureFlags);
    } else {
      return this.http.get(FeatureFlagSource.URL).pipe(
          map((flagsFlatMap) => {
            this.featureFlags = flagsFlatMap;
            return flagsFlatMap;
          })
        );
    }
  }

  routeIsAllowed(childRoute: ActivatedRouteSnapshot): boolean {
    const url = childRoute.url[0] ? childRoute.url[0].path : '';
    const routeIsPresent = this.routeFeatureMapping[url];
    const routeIsAllowed = this.featureFlags[routeIsPresent];
    return (routeIsPresent === undefined) || (routeIsPresent && routeIsAllowed);
  }

  check(flag: string): boolean {
    return this.featureFlags && ((this.featureFlags[flag]  === undefined) || this.featureFlags[flag]);
  }

}

@Injectable({
  providedIn: 'root'
})
export class MockFeatureFlagService {
  initialize() {}
  routeIsAllowed() {return true; }
  check(): boolean { return true; }
}
