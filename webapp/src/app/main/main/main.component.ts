/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { ClrDropdown } from '@clr/angular';

import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';

@Component({
  selector: 'athena-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnInit, OnDestroy {
  @ViewChild('userProfileMenu') userProfileMenu: ClrDropdown;
  alerts: any = [];
  authenticationChange: Subscription;
  userProfileMenuToggleChanges: Subscription;
  authenticated = false;
  username: string;
  personas = Personas;
  personaOptions = PersonaService.getOptions();
  inactivityTimeout: any;

  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private alertService: ErrorAlertService,
    public zone: NgZone,
    private personaService: PersonaService,
    private tourService: TourService
  ) {
    this.authenticationChange = authenticationService.user.subscribe(user => {
      this.authenticated = user.email !== undefined && user.persona !== undefined;
      this.username = user.email;
      this.personaService.currentPersona = user.persona;
    });

    this.alertService.notify
      .subscribe(error => this.addAlert(error));

    this.userProfileMenuToggleChanges = this.tourService.userProfileDropdownChanges$.subscribe((openMenu) => {
      setTimeout(() => {
        this.userProfileMenu.ifOpenService.open = openMenu;
      });
    });

    this.setInactivityTimeout();
  }

  ngOnInit() {
    this.tourService.initialUrl = this.router.url.substr(1);
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
    this.userProfileMenuToggleChanges.unsubscribe();
    clearTimeout(this.inactivityTimeout);
  }

  onPersonaChange(persona: Personas) {
    localStorage.setItem('helen.persona', persona);
    this.personaService.currentPersona = persona;
    location.reload();
  }

  onLogOut() {
    this.authenticationService.logOut();
    this.router.navigate(['']);
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
    this.inactivityTimeout = setTimeout(() => {
      this.authenticationService.logOut();
      this.router.navigate(['auth/login']);
    }, oneHour);
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
