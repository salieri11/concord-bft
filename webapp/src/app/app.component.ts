/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs/Subscription';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from './shared/authentication.service';
import { ErrorAlertService } from './shared/global-error-handler.service';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnDestroy {
  title = 'app';
  alerts: any = [];
  authenticationChange: Subscription;

  authenticated = false;
  username: string;

  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private alertService: ErrorAlertService,
    public zone: NgZone,
    private translate: TranslateService
  ) {
    const browserLang = this.translate.getBrowserLang();
    this.translate.setDefaultLang('en');
    this.translate.use(browserLang);

    this.authenticationChange = authenticationService.user.subscribe(email => {
      this.authenticated = email !== undefined;
      this.username = email;
    });

    this.alertService.notify
      .subscribe(error => this.addAlert(error));
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onLogOut() {
    this.authenticationService.logOut();
    this.router.navigate(['auth', 'log-in']);
  }

  private addAlert(alert: any): void {
    if (alert && alert.message) {
      const alertItem = {
        message: alert.message
      };
      if (this.alerts.indexOf(alertItem) === -1) {
        this.zone.run(() => this.alerts.push(alertItem));
      }
    }
  }
}
