/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject } from 'rxjs/internal/BehaviorSubject';
import { Observable } from 'rxjs/internal/Observable';
import { TranslateService } from '@ngx-translate/core';
import { TourService as NgxTourService, IStepOption } from 'ngx-tour-ngx-popper';

import { Personas, PersonaService } from './persona.service';

@Injectable({
  providedIn: 'root'
})
export class TourService {
  private _initialUrl: string;
  private _initialDashboardUrl: string;
  steps: IStepOption[];

  private userProfileDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userProfileDropdownChanges$: Observable<boolean> = this.userProfileDropdownChangeSubject.asObservable();

  private userActionsDropdownChangeSubject: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
  userActionsDropdownChanges$: Observable<boolean> = this.userActionsDropdownChangeSubject.asObservable();

  constructor(
    private personaService: PersonaService,
    private router: Router,
    private translate: TranslateService,
    private ngxTourService: NgxTourService
  ) {}

  get initialUrl() {
    return this._initialUrl;
  }

  set initialUrl(initialUrl: string) {
    this._initialUrl = initialUrl;
  }

  get initialDashboardUrl() {
    return this._initialDashboardUrl;
  }

  set initialDashboardUrl(initialDashboardUrl: string) {
    this._initialDashboardUrl = initialDashboardUrl;
  }

  openUserProfileMenu() {
    this.userProfileDropdownChangeSubject.next(true);
  }

  closeUserProfileMenu() {
    this.userProfileDropdownChangeSubject.next(false);
  }

  openUserActionsMenu() {
    this.userActionsDropdownChangeSubject.next(true);
  }

  closeUserActionsMenu() {
    this.userActionsDropdownChangeSubject.next(false);
  }

  startTour() {
    this.steps = [
      {
        anchorId: 'onboardingTour.transactionList',
        content: this.translate.instant('tourSteps.dashboard.transactionList.text'),
        title: this.translate.instant('tourSteps.dashboard.transactionList.title'),
        route: 'dashboard'
      },
      {
        anchorId: 'onboardingTour.manageSmartContracts',
        content: this.translate.instant('tourSteps.smartContracts.manageSmartContracts.text'),
        title: this.translate.instant('tourSteps.smartContracts.manageSmartContracts.title'),
        route: 'smart-contracts'
      },
      {
        anchorId: 'onboardingTour.createSmartContract',
        content: this.translate.instant('tourSteps.smartContracts.createSmartContract.text'),
        title: this.translate.instant('tourSteps.smartContracts.createSmartContract.title'),
        route: 'smart-contracts'
      },
      {
        anchorId: 'onboardingTour.userManagement',
        content: this.translate.instant('tourSteps.users.userManagement.text'),
        title: this.translate.instant('tourSteps.users.userManagement.title'),
        route: 'smart-contracts',
        placement: 'right',
        popperSettings: {
          boundariesElement: 'body'
        }
      },
      {
        anchorId: 'onboardingTour.userActions',
        content: this.translate.instant('tourSteps.users.userActions.text'),
        title: this.translate.instant('tourSteps.users.userActions.title'),
        route: 'users',
        placement: 'left'
      },
      {
        anchorId: 'onboardingTour.userSettings',
        content: this.translate.instant('tourSteps.users.userSettings.text'),
        title: this.translate.instant('tourSteps.users.userSettings.title'),
        placement: 'left',
        route: 'users'
      },
      {
        anchorId: 'onboardingTour.downloadCertificate',
        content: this.translate.instant('tourSteps.users.downloadCertificate.text'),
        title: this.translate.instant('tourSteps.users.downloadCertificate.title'),
        route: 'users/settings'
      },
    ];

    if ((this.personaService.currentPersona === Personas.OrgDeveloper )) {
      this.steps.splice(4, 2);
    }

    if ((this.personaService.currentPersona === Personas.OrgUser )) {
      this.steps.splice(3, 3);
    }

    this.ngxTourService.stepShow$.subscribe((event) => {
      switch (event.anchorId) {
        case 'onboardingTour.userActions':
          this.openUserActionsMenu();
          break;
        case 'onboardingTour.userSettings':
          this.openUserProfileMenu();
          break;
      }
    });

    this.ngxTourService.stepHide$.subscribe((event) => {
      switch (event.anchorId) {
        case 'onboardingTour.userActions':
          this.closeUserActionsMenu();
          break;
        case 'onboardingTour.userSettings':
          this.closeUserProfileMenu();
          break;
      }
    });

    this.ngxTourService.end$.subscribe(() => {
      this.router.navigate(['dashboard']);
    });

    this.ngxTourService.initialize(this.steps, {
      prevBtnTitle: this.translate.instant('tourSteps.prevBtnText'),
      nextBtnTitle: this.translate.instant('tourSteps.nextBtnText'),
      endBtnTitle: this.translate.instant('tourSteps.endBtnText'),
      popperSettings: {
        hideOnClickOutside: false
      }
    });

    this.ngxTourService.start();
  }
}
