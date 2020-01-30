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
import { tap, catchError, switchMap, take, finalize, filter } from 'rxjs/operators';
import { AuthenticationService } from './shared/authentication.service';
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
    private authService: AuthenticationService,
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

    return next.handle(this.addTokenToRequest(request, localStorage.getItem('jwtToken'))).pipe(
      catchError((error) => {
        if (error instanceof HttpErrorResponse) {
          switch ((<HttpErrorResponse>error).status) {
            case 401:
              if (request.url === 'api/auth/login') {
                return throwError(error);
              }
              return this.handle401Error(request, next);
            case 400:
              if (error.url.indexOf('api/auth/refresh') !== -1) {
                this.authService.logOut();
              }
              return throwError(error);
            default:
              return throwError(error);
          }
        } else {
          return throwError(error);
        }
      }),
      tap((event: HttpEvent<any>) => {
        // Make sure this is a response from the Ethereum endpoint and that there is an error present
        if (event instanceof HttpResponse && event.url.indexOf('api/concord/eth') !== -1 && event.body.error) {
          return throwError(event.body.error.message);
        }
      }),
    );
  }

  private addTokenToRequest(request: HttpRequest<any>, token: any): HttpRequest<any> {
    return request.clone({ setHeaders: { Authorization: `Bearer ${token}` } });
  }

  private handle401Error(request: HttpRequest<any>, next: HttpHandler) {

    if (!this.isRefreshingToken) {
      this.isRefreshingToken = true;

      // Reset here so that the following requests wait until the token
      // comes back from the refreshToken call.
      this.tokenSubject.next(null);

      return this.authService.refreshToken()
        .pipe(
          switchMap((user) => {
            this.tokenSubject.next(user.token);
            return next.handle(this.addTokenToRequest(request, user.token));
          }),
          catchError(err => {
            if (err) {
              this.authService.logOut();
            }
            return of(err);
          }),
          finalize(() => {
            this.isRefreshingToken = false;
          })
        );
    } else {
      this.isRefreshingToken = false;

      return this.tokenSubject
        .pipe(filter(token => token != null),
          take(1),
          switchMap(token => {
            return next.handle(this.addTokenToRequest(request, token));
          }));
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
