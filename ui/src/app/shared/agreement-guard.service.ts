/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './authentication.service';
import { ErrorAlertService } from './global-error-handler.service';
import { environment } from './../../environments/environment';
import { authRoutes } from './urls.model';

@Injectable()
export class AgreementGuard implements CanActivate {
  env = environment;

  constructor(private router: Router,
    private authenticationService: AuthenticationService,
    private errorService: ErrorAlertService,
    private translateService: TranslateService) {
  }

  async canActivate(): Promise<any> {
    if (this.authenticationService.agreement) { return true; } // check only once
    const agreement = await this.authenticationService.checkForLegalAgreements().toPromise();
    if (!agreement) {
      this.router.navigate([authRoutes.base, authRoutes.onboarding]);
      return false;
    } else {
      return true;
    }
  }

  handleRoutingFailure() {
    this.errorService.add(new Error(this.translateService.instant('globalError.routingError')));
  }
}

