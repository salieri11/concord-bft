/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import {
  HttpRequest,
  HttpHandler,
  HttpEvent,
  HttpInterceptor, HttpResponse, HttpErrorResponse, HttpClient
} from '@angular/common/http';
import { Router } from '@angular/router';

import { Observable, throwError, of, BehaviorSubject } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { ErrorAlertService } from './shared/global-error-handler.service';
import { environment } from './../environments/environment';
import { interceptDAMLCalls } from './app-intercept.daml.proxy';
import { FeatureFlagService, FeatureFlags } from './shared/feature-flag.service';

/**
 * Checks for successful HTTP responses (status code 2XX) that are actually errors and throws.
 *
 * The Ethereum API endpoint is currently returning error responses as HTTP status code 200. Once this is resolved,
 * this can be removed.
 */
@Injectable()
export class RequestInterceptor implements HttpInterceptor {
  env = environment;
  isRefreshingToken: boolean = false;
  tokenSubject: BehaviorSubject<string> = new BehaviorSubject<string>(null);
  cspErrors = [];
  constructor(
    private router: Router,
    private http: HttpClient,
    private alertService: ErrorAlertService,
    private featureFlagService: FeatureFlagService
  ) { }

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (this.env.csp) {

      /**
       * ! Temporary
       * requires: feature-flag, daml_contracts_explorer true
       * until helen supports authenticated proxy pass to DAML json-api
       */
      if (this.featureFlagService.check(FeatureFlags.daml_contracts_explorer)) {
        // intercept, proxy pass to json-api
        const result = interceptDAMLCalls(request, this.http);
        if (result) { return result; }
      }

      return next.handle(request).pipe(

        catchError((error) => {
          if (error instanceof HttpErrorResponse) {

            /**
             * ! Temporary
             * requires: feature-flag, daml_contracts_explorer true
             * until helen supports authenticated proxy pass to DAML json-api
             */
            if (this.featureFlagService.check(FeatureFlags.daml_contracts_explorer)) {
              // Intercept json-api error
              if (request.url === '/daml-json-api/contracts/search') {
                return of(new HttpResponse<any>({
                  status: 200,
                  body: { result: { error: 'Cannot connect to JSON API endpoint of DAML Ledger'} }
                }));
              }
            }

            // Used for distinguishing vCenter zone registration test from validation vs connection error
            if (request.url.indexOf('api/blockchains/zones?action=test') >= 0
                && error.error.error_message.indexOf('io.grpc.StatusRuntimeException: UNKNOWN') >= 0) {
              return of(new HttpResponse<any>({ status: 206, body: { result: 'UNKNOWN' } }));
            }

            switch ((<HttpErrorResponse>error).status) {
              case 401:
                this.cspErrors.push(error);
                if (window.location.search.indexOf('org_link') !== -1) {
                  window.location.href = `https://${window.location.host}/api/oauth/login${window.location.search}`;
                  return;
                }
                // Store last location, so that on redirect we redirect them to the last used location.
                // We are adding a date so that we only use this for recent redirects that have happened
                // in the last couple minutes.
                localStorage.setItem('lastLocation', `${new Date()}--${window.location.pathname}`);

                this.cspErrors = [];
                window.location.href = this.env.loginPath;
                return throwError(error);
              case 403:
                this.router.navigate(['/forbidden']);
                return throwError(error);

              case 500:
                this.alertService.add(error);
                return throwError(error);

              default:
                return throwError(error);
            }
          } else {
            return throwError(error);
          }
         })
      );
    }
  }
}

@Injectable()
export class MockRequestInterceptor implements HttpInterceptor {

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    return next.handle(request).pipe(
      catchError((e) => {
        return of(e);
      })
    );
  }

}
