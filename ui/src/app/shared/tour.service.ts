/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/internal/BehaviorSubject';
import { Observable } from 'rxjs/internal/Observable';
import { TranslateService } from '@ngx-translate/core';
import { TourService as NgxTourService, IStepOption } from 'ngx-tour-ngx-popper';

import { Personas, PersonaService } from './persona.service';
import { BlockchainService } from './blockchain.service';

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
    private translate: TranslateService,
    private ngxTourService: NgxTourService,
    private blockchainService: BlockchainService
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
    const bId = this.blockchainService.selectedBlockchain.id;
    this.steps = [
      {
        anchorId: 'onboardingTour.dashStats',
        content: this.translate.instant('tourSteps.dashboard.dashStats.text'),
        title: this.translate.instant('tourSteps.dashboard.dashStats.title'),
        route: `${bId}/dashboard`,
        placement: 'bottom'
      },
      {
        anchorId: 'onboardingTour.contractList',
        content: this.translate.instant('tourSteps.dashboard.contractList.text'),
        title: this.translate.instant('tourSteps.dashboard.contractList.title'),
        route: `${bId}/dashboard`
      },
      {
        anchorId: 'onboardingTour.manageSmartContracts',
        content: this.translate.instant('tourSteps.smartContracts.manageSmartContracts.text'),
        title: this.translate.instant('tourSteps.smartContracts.manageSmartContracts.title'),
        route: `${bId}/smart-contracts`,
        placement: 'top'
      },
      {
        anchorId: 'onboardingTour.createSmartContract',
        content: this.translate.instant('tourSteps.smartContracts.createSmartContract.text'),
        title: this.translate.instant('tourSteps.smartContracts.createSmartContract.title'),
        route: `${bId}/smart-contracts`
      },
      {
        anchorId: 'onboardingTour.userManagement',
        content: this.translate.instant('tourSteps.users.userManagement.text'),
        title: this.translate.instant('tourSteps.users.userManagement.title'),
        route: `${bId}/smart-contracts`,
        placement: 'right',
        popperSettings: {
          boundariesElement: 'body'
        }
      },
      {
        anchorId: 'onboardingTour.userActions',
        content: this.translate.instant('tourSteps.users.userActions.text'),
        title: this.translate.instant('tourSteps.users.userActions.title'),
        route: `${bId}/users`,
        placement: 'left'
      }
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

  startContractTour() {
    this.steps = [
      {
        anchorId: 'contract.deployed',
        title: this.translate.instant('smartContracts.tour.deployed.title'),
        content: this.translate.instant('smartContracts.tour.deployed.content'),
        placement: 'right',
      },
      {
        anchorId: 'contract.availableActions',
        title: this.translate.instant('smartContracts.tour.availableActions.title'),
        content: this.translate.instant('smartContracts.tour.availableActions.content'),
      },
      {
        anchorId: 'contract.call',
        title: this.translate.instant('smartContracts.tour.call.title'),
        content: this.translate.instant('smartContracts.tour.call.content'),
      },
      {
        anchorId: 'contract.send',
        title: this.translate.instant('smartContracts.tour.send.title'),
        content: this.translate.instant('smartContracts.tour.send.content'),
      },
      {
        anchorId: 'contract.tabs',
        title: this.translate.instant('smartContracts.tour.tabs.title'),
        content: this.translate.instant('smartContracts.tour.tabs.content'),
        placement: 'bottom',
      },
    ];

    this.ngxTourService.initialize(this.steps, {
      prevBtnTitle: this.translate.instant('tourSteps.prevBtnText'),
      nextBtnTitle: this.translate.instant('tourSteps.nextBtnText'),
      popperSettings: {
        hideOnClickOutside: false
      }
    });

    this.ngxTourService.start();
  }
}
