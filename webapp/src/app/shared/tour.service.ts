/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/internal/BehaviorSubject';
import { Observable } from 'rxjs/internal/Observable';
import { JoyrideService } from 'ngx-joyride';

import { Personas, PersonaService } from './persona.service';
import { Router } from '@angular/router';

@Injectable({
  providedIn: 'root'
})
export class TourService {
  private userProfileDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userProfileDropdownChanges$: Observable<boolean> = this.userProfileDropdownChangeSubject.asObservable();

  private scrollTransactionListSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollTransactionListSubjectChanges$: Observable<boolean> = this.scrollTransactionListSubject.asObservable();

  private scrollMapSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(null);
  scrollMapSubjectChanges$: Observable<boolean> = this.scrollMapSubject.asObservable();

  isUserProfileMenuOpen = false;

  constructor(private personaService: PersonaService,
              private joyrideService: JoyrideService,
              private router: Router) {
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
    this.router.navigate(['/dashboard']);
    this.scrollToMap();
    this.joyrideService.startTour(
      {
        steps: steps
      }
    );
  }
}
