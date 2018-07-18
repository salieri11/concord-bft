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
  private userProfileDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userProfileDropdownChanges$: Observable<boolean> = this.userProfileDropdownChangeSubject.asObservable();

  private userActionsDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userActionsDropdownChanges$: Observable<boolean> = this.userActionsDropdownChangeSubject.asObservable();

  private scrollSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollSubjectChanges$: Observable<boolean> = this.scrollSubject.asObservable();

  isUserProfileMenuOpen = false;
  isUserActionsMenuOpen = false;

  constructor(private personaService: PersonaService,
              private joyrideService: JoyrideService) {
  }

  toggleUserProfileMenu() {
    this.isUserProfileMenuOpen = !this.isUserProfileMenuOpen;
    this.userProfileDropdownChangeSubject.next(this.isUserProfileMenuOpen);
  }


  toggleUserActionsMenu() {
    this.isUserActionsMenuOpen = !this.isUserActionsMenuOpen;
    this.userActionsDropdownChangeSubject.next(this.isUserActionsMenuOpen);
  }

  scrollToElement() {
    this.scrollSubject.next(true);
  }

  startTour(initialUrl: string) {
    const steps: any[] = [
      'nodeStatus@dashboard',
      'transactionList@dashboard',
      'manageSmartContracts@smart-contracts',
      'createSmartContract@smart-contracts',
      `userManagement@${initialUrl}`,
      'userActions@users',
      'userSettings@dashboard',
      'downloadCertificate@users/settings'
    ];
    if ((this.personaService.currentPersona === Personas.OrgDeveloper || this.personaService.currentPersona === Personas.OrgUser)) {
      steps.splice(4, 2);
    }

    this.joyrideService.startTour(
      {
        steps: steps,
        stepDefaultPosition: 'top'
      }
    );
  }
}
