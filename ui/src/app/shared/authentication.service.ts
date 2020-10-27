/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';

import { environment } from './../../environments/environment';
import { Personas, PersonaService } from './persona.service';
import { OrgService } from '../orgs/shared/org.service';
import { testController } from '../../test.controller'; // ! temp: must be overriden by actual mock

export interface UserAuthResponse {
  auth_token: string;
  last_login: number;
  email: string;
}

@Injectable()
export class AuthenticationService {
  readonly user: Observable<any>;
  agreement: boolean = false;
  redirectUrl: string;
  accessToken: string;
  logoutPath: string = '/api/oauth/logout';
  parsedToken: any;
  orgProps: any;

  constructor(
    private personaService: PersonaService,
    private orgService: OrgService,
    private http: HttpClient,
    private router: Router,
  ) {}


  get currentUser() {
    return {
      email: localStorage['helen.email'],
      consortium_id: localStorage['helen.consortium_id'],
      persona: localStorage['helen.persona'],
      user_id: localStorage['helen.user_id'],
      wallet_address: localStorage['helen.wallet_address']
    };
  }

  getAccessToken(): Observable<UserAuthResponse> {
    const url = 'api/oauth/token';
    return this.http.get<UserAuthResponse>(url).pipe(
      map(response => {
        this.accessToken = response.auth_token;
        this.parsedToken = this.parseJwt(this.accessToken);
        this.setPersona(this.parsedToken.perms);

        return response;
      }),
      mergeMap(response => this.resolveOrgProperties(response))
    );
  }

  saveLastLocationAndReAuth() {
    localStorage.setItem('lastLocation', `${new Date()}--${window.location.pathname}`);
    window.location.href = environment.loginPath;
  }

  refreshToken(): Observable<any> {
    const url = 'api/auth/token';
    return this.http.post<{ refresh_token: string }>(url, {
      refresh_token: localStorage.getItem('jwtRefreshToken')
    }).pipe(
      map(response => {
        this.setToken(response);
        return response;
      })
    );
  }

  setRedirectPath() {
    const redirect = this.router.url;
    const disallowedRedirects = ['/auth/login'];

    if (disallowedRedirects.indexOf(redirect) === -1) {
      this.redirectUrl = redirect;
    }
  }


  checkForLegalAgreements(): Observable<boolean> {
    // TODO: implement mock service instead or returning something different during unit tests.
    if (testController.forTesting) { return of(true); }
    return this.http.get<Array<any>>('api/organizations/agreements').pipe(
      map((response) => {
        this.agreement = response.length !== 0;
        return this.agreement;
      }),
    );
  }

  getLegalAgreement(): Observable<any> {
    return this.http.get('static/agreements/tos.txt', {responseType: 'text'});
  }

  acceptLegalAgreement(params: any): Observable<any> {
    return this.http.post<any>('api/organizations/agreements', params);
  }

  private setPersona(perms: string[]) {
    perms.forEach(perm => {
      if (perm.startsWith('external')) {
        const permList = perm.split('/');

        switch (permList[2]) {
          case 'vmbc-system:admin':
            this.personaService.currentPersonas.push(Personas.SystemsAdmin);
            break;
          case 'vmbc-system:infra':
            this.personaService.currentPersonas.push(Personas.SystemsInfra);
            break;
          case 'vmbc-org:user':
            this.personaService.currentPersonas.push(Personas.OrgUser);
            break;
          case 'vmbc-org:dev':
            this.personaService.currentPersonas.push(Personas.OrgDeveloper);
            break;
          case 'vmbc-org:admin':
            this.personaService.currentPersonas.push(Personas.OrgAdmin);
            break;
          case 'vmbc-consortium:admin':
            this.personaService.currentPersonas.push(Personas.ConsortiumAdmin);
            break;
          case 'vmbc-consortium:operator':
            this.personaService.currentPersonas.push(Personas.ConsortiumOperator);
            break;
          case 'vmbc-consortium:participant':
            this.personaService.currentPersonas.push(Personas.ConsortiumParticipant);
            break;

          default:
            break;
        }
      }
    });
  }
  private setToken(response): void {
    const expiresAt = Date.now() + response.token_expires;
    const refreshExpiresAt = Date.now() + (response.token_expires * 2);

    localStorage.setItem('jwtToken', response.token);
    localStorage.setItem('jwtRefreshToken', response.refresh_token);
    localStorage.setItem('jwtTokenExpiresAt', expiresAt.toString());
    localStorage.setItem('jwtRefreshTokenExpiresAt', refreshExpiresAt.toString());
  }

  private parseJwt(token: string): any {
    const base64Url = token.split('.')[1];
    const base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
    const jsonPayload = decodeURIComponent(atob(base64).split('').map(function (c) {
      return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
    }).join(''));

    return JSON.parse(jsonPayload);
  }

  private resolveOrgProperties(response): Observable<any> {
    const orgId = this.parsedToken.context_name;

    return this.orgService.getDetail(orgId).pipe(
      map(res => {
        if (res.organization_properties === null) {
          res.organization_properties = {max_chains: 1};
        }
        if (res.organization_properties.max_chains === undefined) {
          res.organization_properties.max_chains = 1;
        }
        res.organization_properties.max_chains = Number(res.organization_properties.max_chains);
        this.orgProps = res.organization_properties;

        return response;
      })
    );
  }

}


export class MockAuthenticationService {
  readonly user: Observable<any>;
  agreement: boolean = false;
  redirectUrl: string;
  accessToken: string;
  logoutPath: string = '/api/oauth/logout';
  parsedToken: any;
  orgProps: any;

  get currentUser() { return {}; }

  isAuthenticated() { return true; }
  getAccessToken() { return of({}); }
  saveLastLocationAndReAuth() {}
  refreshToken() { return of({}); }
  logIn() { return of({}); }
  changePassword() { return of({}); }
  logOut() {}
  setRedirectPath() {}
  checkForLegalAgreements() { return of(true); }
  getLegalAgreement() { return of('agreement_text'); }
  acceptLegalAgreement() { return of({}); }
  handleLogin() {}
  getCountryList() { return []; }

}
