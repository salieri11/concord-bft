/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, TemplateRef, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { ClrDropdown } from '@clr/angular';

import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { TaskManagerService } from '../../shared/task-manager.service';
import { TourService } from '../../shared/tour.service';

@Component({
  selector: 'athena-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnDestroy {
  @ViewChild('userProfileMenu') userProfileMenu: ClrDropdown;
  @ViewChild('transactionListDiv') transactionListDiv: TemplateRef<any>;
  alerts: any = [];
  authenticationChange: Subscription;

  authenticated = false;
  username: string;
  personas = Personas;
  personaOptions = PersonaService.getOptions();

  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private alertService: ErrorAlertService,
    public zone: NgZone,
    private personaService: PersonaService,
    private taskManagerService: TaskManagerService,
    private tourService: TourService
  ) {
    this.authenticationChange = authenticationService.user.subscribe(user => {
      this.authenticated = user.email !== undefined && user.persona !== undefined;
      this.username = user.email;
      this.personaService.currentPersona = user.persona;
    });

    this.alertService.notify
      .subscribe(error => this.addAlert(error));

    this.tourService.userProfileDropdownChanges$.subscribe((openMenu) => {
      setTimeout(() => {
        this.userProfileMenu.ifOpenService.open = openMenu;
      });
    });

  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onPersonaChange(persona: Personas) {
    localStorage.setItem('helen.persona', persona);
    this.personaService.currentPersona = persona;
    location.reload();
  }

  onLogOut() {
    this.authenticationService.logOut();
    this.router.navigate(['auth', 'log-in']);
  }

  onResetTasks() {
    this.taskManagerService.resetTasks();
  }

  startTour() {
    this.tourService.startTour();
  }

  onNext() {
   // this.closeUserProfileMenu();
    this.tourService.toggleUserProfileMenu();
  }

  onPrev() {
    // this.closeUserProfileMenu();
    this.tourService.toggleUserProfileMenu();
    this.openUserActionsMenu();
  }

  openUserActionsMenu() {
    this.tourService.toggleUserActionsMenu();
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
