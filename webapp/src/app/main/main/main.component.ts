/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

@Component({
  selector: 'app-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnDestroy {
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
  ) {
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
