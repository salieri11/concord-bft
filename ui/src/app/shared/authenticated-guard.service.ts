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
import { FeatureFlagService } from './feature-flag.service';
import { environment } from '../../environments/environment';
import { QueryParams, mainFragments, mainRoutes, authRoutes } from './urls.model';
import { RouteService } from './route.service';


@Injectable()
export class AuthenticatedGuard implements CanActivateChild, CanActivate {
  env = environment;

  constructor(
    private router: Router,
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private errorService: ErrorAlertService,
    private translateService: TranslateService,
    private featureFlagService: FeatureFlagService,
    private routeService: RouteService,
  ) { }

  async canActivateChild(
    childRoute: ActivatedRouteSnapshot,
  ) {
    const personasAllowed: Personas[] = childRoute.component ? (childRoute.component as any).personasAllowed : null;
    await this.featureFlagService.initialize().toPromise();

    if (!this.featureFlagService.routeIsAllowed(childRoute)) {
      this.router.navigate([mainRoutes.forbidden]);
      return false;
    } else if (this.env.csp) {
      return true;
    } else if (localStorage.getItem('changePassword') || !this.authenticationService.isAuthenticated()) {
      this.router.navigate([authRoutes.base, authRoutes.login]);
      return false;
    } else if (personasAllowed && !this.personaService.hasAuthorization(personasAllowed)) {
      this.handleRoutingFailure();
      this.router.navigate(['/' + mainRoutes.forbidden]);
      return false;
    } else {
      return true;
    }
  }

  async canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot) {
    if (!this.env.csp) { return true; }
    if (this.authenticationService.accessToken) { return true; } // already logged in

    const auth = await this.authenticationService.getAccessToken().toPromise();
    await this.routeService.resolveConsortium();

    if (this.isNewUser(route, state, auth)) {
      this.routeService.redirectToDefault(mainFragments.welcome);
      return false;
    } else if (state.url.indexOf(authRoutes.loginReturn) !== -1) {
      return this.routeService.loginReturnHandler();
    } else if (route.url.length === 0) { // emptry string path '', redirect to dashboard.
      this.routeService.redirectToDefault();
      return false;
    }
    return true;
  }

  private handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }

  private isNewUser(
    route: ActivatedRouteSnapshot,
    state: RouterStateSnapshot,
    auth: UserAuthResponse
  ) {
    return ((state.url.indexOf(authRoutes.loginReturn) !== -1
      && route.queryParams[QueryParams.userKey] === QueryParams.userNewValue) ||
      auth.last_login === 0);
  }
}

