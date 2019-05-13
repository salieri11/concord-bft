/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanActivateChild, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './authentication.service';
import { Personas, PersonaService } from './persona.service';
import { ErrorAlertService } from './global-error-handler.service';
import { BlockchainService, BlockchainResponse } from './blockchain.service';

@Injectable()
export class AuthenticatedGuard implements CanActivateChild, CanActivate {
  constructor(
    private router: Router,
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private errorService: ErrorAlertService,
    private translateService: TranslateService,
    private blockchainService: BlockchainService
  ) { }

  canActivateChild(childRoute: ActivatedRouteSnapshot): boolean {
    const personasAllowed: Personas[] = childRoute.component ? (childRoute.component as any).personasAllowed : null;
    const blockchain: BlockchainResponse = this.blockchainService.selectedBlockchain;

    if (localStorage.getItem('changePassword') || !this.authenticationService.isAuthenticated()) {
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

  canActivate(route: ActivatedRouteSnapshot): boolean {
    const personasAllowed: Personas[] = (route.component as any).personasAllowed;
    const blockchain: BlockchainResponse = this.blockchainService.selectedBlockchain;

    if (localStorage.getItem('changePassword') || this.authenticationService.isAuthenticated()) {
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

  handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }
}

