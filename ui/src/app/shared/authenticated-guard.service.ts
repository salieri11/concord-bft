/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import {
  ActivatedRouteSnapshot,
  RouterStateSnapshot,
  CanActivate,
  CanActivateChild,
  Router
} from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './authentication.service';
import { UserAuthResponse } from '../users/shared/user.model';
import { Personas, PersonaService } from './persona.service';
import { ErrorAlertService } from './global-error-handler.service';
import { BlockchainService } from './blockchain.service';
import { BlockchainResponse } from './blockchain.model';
import { environment } from '../../environments/environment';
import { QueryParams } from './urls.model';


@Injectable()
export class AuthenticatedGuard implements CanActivateChild, CanActivate {
  env = environment;

  constructor(
    private router: Router,
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private errorService: ErrorAlertService,
    private translateService: TranslateService,
    private blockchainService: BlockchainService,
  ) { }

  canActivateChild(childRoute: ActivatedRouteSnapshot) {
    const personasAllowed: Personas[] = childRoute.component ? (childRoute.component as any).personasAllowed : null;
    const blockchain: BlockchainResponse = this.blockchainService.selectedBlockchain;

    if (this.env.csp) {
      return true;
    } else if (localStorage.getItem('changePassword') || !this.authenticationService.isAuthenticated()) {
      this.router.navigate(['auth', 'login']);
      return false;
    } else if (personasAllowed && !this.personaService.hasAuthorization(personasAllowed)) {
      this.handleRoutingFailure();
      this.router.navigate(['/' + blockchain.id, 'dashboard']);
      return false;
    } else {
      return true;
    }
  }

  async canActivate(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot
  ) {

    if (this.env.csp) {
      if (!this.authenticationService.accessToken) {
        const auth = await this.authenticationService.getAccessToken().toPromise();

        if (this.isNewUser(route, state, auth)) {
          this.router.navigate(['/', 'welcome'], { fragment: 'welcome' });
        }
      }
      return true;

    } else {
      return true;
    }
  }

  private handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }

  private isNewUser(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot,
    auth: UserAuthResponse
  ) {
    return ((state.url.indexOf(this.authenticationService.loginReturnPath) !== -1
      && route.queryParams[QueryParams.userKey] === QueryParams.userNewValue) ||
      auth.last_login === 0);
  }
}

