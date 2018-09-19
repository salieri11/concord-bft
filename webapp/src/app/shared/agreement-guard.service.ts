/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { CanActivate, Router, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './authentication.service';
import { ErrorAlertService } from './global-error-handler.service';

@Injectable()
export class AgreementGuard implements CanActivate {

  constructor(private router: Router,
              private authenticationService: AuthenticationService,
              private errorService: ErrorAlertService,
              private translateService: TranslateService) {
  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): any {
    const url: string = state.url;

    if (this.authenticationService.agreement.accepted) {
      return true;
    }

    return this.authenticationService.checkForLegalAgreements()
      .subscribe(agreement => {
        if (!agreement['accepted']) {
          this.router.navigate(['auth', 'onboarding']);
        } else {
          this.router.navigate([url]);
        }
        return agreement['accepted'];
      });
  }

  handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }
}

