/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/internal/BehaviorSubject';
import { Observable } from 'rxjs/internal/Observable';
import { JoyrideService } from 'ngx-joyride';

import { Personas, PersonaService } from './persona.service';

@Injectable({
  providedIn: 'root'
})
export class TourService {
  private _initialUrl: string;
  steps: string[];

  private userProfileDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userProfileDropdownChanges$: Observable<boolean> = this.userProfileDropdownChangeSubject.asObservable();

  private scrollTransactionListSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollTransactionListSubjectChanges$: Observable<boolean> = this.scrollTransactionListSubject.asObservable();

  private scrollMapSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollMapSubjectChanges$: Observable<boolean> = this.scrollMapSubject.asObservable();

  isUserProfileMenuOpen = false;

  constructor(private personaService: PersonaService,
              private joyrideService: JoyrideService) {
  }

  get initialUrl() {
    return this._initialUrl;
  }

  set initialUrl(initialUrl: string) {
    this._initialUrl = initialUrl;
  }

  toggleUserProfileMenu() {
    this.isUserProfileMenuOpen = !this.isUserProfileMenuOpen;
    this.userProfileDropdownChangeSubject.next(this.isUserProfileMenuOpen);
  }

  scrollToTransactionList() {
    this.scrollTransactionListSubject.next(true);
  }

  scrollToMap() {
    this.scrollMapSubject.next(true);
  }

  startTour() {
    this.steps = [
      'nodeStatus@dashboard',
      'transactionList@dashboard',
      'manageSmartContracts@smart-contracts',
      'createSmartContract@smart-contracts',
      `userManagement@${this.initialUrl}`,
      'userActions@users',
      'userSettings@dashboard',
      'downloadCertificate@users/settings'
    ];

    if ((this.personaService.currentPersona === Personas.OrgDeveloper )) {
      this.steps.splice(4, 2);
    }

    if ((this.personaService.currentPersona === Personas.OrgUser )) {
      this.steps.splice(3, 3);
    }

    this.scrollToMap();
    this.joyrideService.startTour(
      {
        steps: this.steps
      }
    );
  }
}
