/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import {
  HttpRequest,
  HttpHandler,
  HttpEvent,
  HttpInterceptor, HttpResponse, HttpErrorResponse
} from '@angular/common/http';
import { Observable, throwError, of, BehaviorSubject } from 'rxjs';
import { tap, catchError, switchMap, take, finalize, filter } from 'rxjs/operators';
import { AuthenticationService } from './shared/authentication.service';
/**
 * Checks for successful HTTP responses (status code 2XX) that are actually errors and throws.
 *
 * The Ethereum API endpoint is currently returning error responses as HTTP status code 200. Once this is resolved,
 * this can be removed.
 */
@Injectable()
export class RequestInterceptor implements HttpInterceptor {
  isRefreshingToken: boolean = false;
  tokenSubject: BehaviorSubject<string> = new BehaviorSubject<string>(null);

  constructor(private authService: AuthenticationService) { }

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
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
                if ( error.url.indexOf('api/auth/refresh' ) !== -1 ) {
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
