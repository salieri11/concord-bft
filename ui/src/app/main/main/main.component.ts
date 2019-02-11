/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { Personas } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';


@Component({
  selector: 'concord-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnInit, OnDestroy {
  alerts: any = [];
  personas = Personas;
  inactivityTimeout: any;

  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private alertService: ErrorAlertService,
    public zone: NgZone,
    private tourService: TourService,
  ) {

    this.alertService.notify
      .subscribe(error => this.addAlert(error));
    this.setInactivityTimeout();
  }

  ngOnInit() {
    this.tourService.initialUrl = this.router.url.substr(1);
  }

  ngOnDestroy(): void {
    this.deregisterWindowListeners();
  }


  private setInactivityTimeout() {
    // If the user is inactive for oneHour we log them
    // out the expiring jwt token isn't enough to handle this
    // because we have polling on the dashboard that will continuously
    // refresh the token.
    const resetTimer = this.resetTimer.bind(this);
    window.onload = resetTimer;
    window.onmousemove = resetTimer;
    window.onmousedown = resetTimer;
    window.onclick = resetTimer;
    window.onscroll = resetTimer;
    window.onkeypress = resetTimer;
  }

  private resetTimer() {
    const oneHour = 3600000;
    clearTimeout(this.inactivityTimeout);
    // e2e tests wait for the Zone to stabilize. This timeout stalls out protractor if not
    // run outside of angular: https://www.protractortest.org/#/timeouts
    this.zone.runOutsideAngular(() => {
      this.inactivityTimeout = setTimeout(() => {
        this.zone.run(() => {
          this.authenticationService.logOut();
        });
      }, oneHour);
    });
  }

  private deregisterWindowListeners() {
    clearTimeout(this.inactivityTimeout);
    window.onload = undefined;
    window.onmousemove = undefined;
    window.onmousedown = undefined;
    window.onclick = undefined;
    window.onscroll = undefined;
    window.onkeypress = undefined;
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
