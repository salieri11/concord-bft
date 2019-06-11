/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import { environment } from '../../../environments/environment';
import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { BlockchainService, BlockchainResponse } from '../../shared/blockchain.service';
import { Personas } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';

import { BlockchainWizardComponent } from '../../shared/components/blockchain-wizard/blockchain-wizard.component';
import { SetupModalComponent } from '../setup-modal/setup-modal.component';
import { DeployingInterstialComponent } from '../deploying-interstitial/deploying-interstitial.component';


@Component({
  selector: 'concord-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnInit, OnDestroy {
  @ViewChild('welcomeModal') welcomeModal: SetupModalComponent;
  @ViewChild('blockchainWizard') blockchainWizard: BlockchainWizardComponent;
  @ViewChild('deployLoader') deployLoader: DeployingInterstialComponent;

  alerts: any = [];
  personas = Personas;
  inactivityTimeout: any;
  navDisabled = false;
  navOption: string;
  openDeployDapp: boolean;
  routerFragmentChange: Subscription;
  blockchains: BlockchainResponse[] = [];
  selectedConsortium: string;
  enableRouterOutlet: boolean = true;

  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private route: ActivatedRoute,
    private alertService: ErrorAlertService,
    public zone: NgZone,
    private tourService: TourService,
    private blockchainService: BlockchainService
  ) {
    const consortiumId = this.route.snapshot.params['consortiumId'];
    this.selectedConsortium = this.blockchainService.select(consortiumId);

    this.alertService.notify
      .subscribe(error => this.addAlert(error));

    if (!environment.csp) {
      this.setInactivityTimeout();
    }

    this.blockchainService.set(
      this.selectedConsortium
    ).subscribe(() => {
      if (this.blockchainService.blockchainId) {
        this.router.navigate(['/', this.blockchainService.blockchainId, 'dashboard']);
      }
    });
  }

  ngOnInit() {
    this.tourService.initialUrl = this.router.url.substr(1);

    this.blockchainService.notify
      .subscribe(notification => this.handleConsortiumNotification(notification));

    this.routerFragmentChange = this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'welcome':
          this.blockchainWizard.close();
          this.welcome();
          break;
        case 'deploy':
          this.welcomeModal.close();
          this.openDeployWizard();
          break;
        default:
          break;
      }
    });

    this.route.params.subscribe(param => {
      if (param.consortiumId && param.consortiumId === 'login-return') {
        // Do nothing
      } else if (param.consortiumId && param.consortiumId.length > 8) {
        this.selectedConsortium = param.consortiumId;
        this.blockchainService.select(this.selectedConsortium);
      }
    });

    this.blockchainWizard.setupComplete.subscribe(
      response => {
        this.router.navigate(['/deploying'], { replaceUrl: true });
        this.enableRouterOutlet = false;
        this.deployLoader.startLoading(response);
      }
    );
  }

  ngOnDestroy(): void {
    this.routerFragmentChange.unsubscribe();
    this.deregisterWindowListeners();
  }

  consortiumChange() {
    this.enableRouterOutlet = false;
    this.navDisabled = false;
    this.deployLoader.showInterstitial = false;

    this.router.navigate([`/${this.selectedConsortium}`, 'dashboard'])
      .then(() => {
        // This is to refresh all child components
        setTimeout(() => {
          this.enableRouterOutlet = true;
        }, 10);
      });
  }

  deployDapp() {
    this.openDeployDapp = true;
  }

  welcome(): void {
    this.welcomeModal.open();
  }

  openDeployWizard() {
    this.blockchainWizard.open();
  }


  private handleConsortiumNotification(notification: any): void {
    if (!notification) { return; }

    switch (notification.message) {
      case 'deploying':
        this.navDisabled = true;
        break;

      case 'deployed':
        this.blockchains = this.blockchainService.blockchains;
        this.navDisabled = false;
        this.enableRouterOutlet = true;
        break;

      case 'non-deployed':
        this.navDisabled = true;
        this.navOption = 'show-deploy';
        break;

      default:
        break;
    }

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
