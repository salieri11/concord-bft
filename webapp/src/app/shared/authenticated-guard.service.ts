/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanActivateChild, Router } from '@angular/router';

import { AuthenticationService } from './authentication.service';
import { PersonaService } from './persona.service';
import { ErrorAlertService } from './global-error-handler.service';
import { TranslateService } from '@ngx-translate/core';

@Injectable()
export class AuthenticatedGuard implements CanActivateChild, CanActivate {
  constructor(private router: Router,
              private authenticationService: AuthenticationService,
              private personaService: PersonaService,
              private errorService: ErrorAlertService,
              private translateService: TranslateService) {
  }

  canActivateChild(): boolean {
    if (!this.authenticationService.isAuthenticated()) {
      this.router.navigate(['auth', 'log-in']);
      return false;
    } else {
      return true;
    }
  }

  canActivate(route: ActivatedRouteSnapshot): boolean {
    const personasAllowed: string[] = (route.component as any).personasAllowed;

    if (!this.authenticationService.isAuthenticated()) {
      this.router.navigate(['auth', 'log-in']);
      return false;
    } else if (personasAllowed && !this.personaService.hasAuthorization(personasAllowed)) {
      this.handleRoutingFailure();
      this.router.navigate(['dashboard']);
      return false;
    } else {
      return true;
    }
  }

  handleRoutingFailure() {
    this.translateService.get('globalError.routingError').subscribe((message) => {
      this.errorService.add(new Error(message));
    });
  }
}

