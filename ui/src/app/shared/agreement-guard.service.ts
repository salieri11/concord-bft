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
    if (this.env.csp) {
      if (url.indexOf('org_link') !== -1) {
        window.location.href = `https://${window.location.host}${environment.loginPath}${window.location.search}`;
        return false;
      } else if (url === '/') {
        window.location.href = `https://${window.location.host}${environment.loginPath}`;
        return false;
      }

      return true;
    } else if (this.authenticationService.agreement.accepted) {
      return true;
    }

    return this.authenticationService.checkForLegalAgreements()
      .subscribe(agreement => {
        if (!agreement['accepted']) {
          this.router.navigate(['auth', 'onboarding']);
        } else if (route) {
          this.router.navigateByUrl(url);
        }
        return agreement['accepted'];
      });
  }

  handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }
}

