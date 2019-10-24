/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { CanActivate, Router, ActivatedRouteSnapshot, RouterStateSnapshot } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './authentication.service';
import { ErrorAlertService } from './global-error-handler.service';
import { environment } from './../../environments/environment';

@Injectable()
export class AgreementGuard implements CanActivate {
  env = environment;

  constructor(private router: Router,
    private authenticationService: AuthenticationService,
    private errorService: ErrorAlertService,
    private translateService: TranslateService) {
  }

  canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): any {
    const url: string = state.url;
    if (this.authenticationService.agreement) {
      return true;
    }

    return this.authenticationService.checkForLegalAgreements()
      .subscribe(agreement => {
        if (!agreement) {
          this.router.navigate(['auth', 'onboarding']);
        } else if (route) {
          this.router.navigateByUrl(url);
        }
        return agreement;
      });
  }

  handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }
}

