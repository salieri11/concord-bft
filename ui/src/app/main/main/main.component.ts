/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { HttpErrorResponse } from '@angular/common/http';
import { Subscription } from 'rxjs';

import { environment } from '../../../environments/environment';
import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';

import { BlockchainResponse } from '../../blockchain/shared/blockchain.model';
import { External, mainRoutes, uuidRegExp } from '../../shared/urls.model';
import { OrgProperties } from '../../orgs/shared/org.model';
<<<<<<< HEAD
import { ZoneType } from '../../zones/shared/zones.model';
=======
import { OnPremisesModalComponent } from '../../blockchain/on-premises-modal/on-premises-modal.component';
>>>>>>> Contextual Help

import { ClrModal } from '@clr/angular';
import { RouteService } from '../../shared/route.service';
import { ContextualHelpService } from './../../shared/contextual-help.service';

@Component({
  selector: 'concord-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.scss']
})
export class MainComponent implements OnInit, OnDestroy {
  @ViewChild('welcomeModal', { static: true }) welcomeModal: ClrModal;

  alerts: any = [];
  personas = Personas;
  inactivityTimeout: any;
  navDisabled = false;
  navOption: string;
  openDeployDapp: boolean;
  sidemenuVisible = true;
  env: any;
  showErrorMessage: boolean = false;
  error: HttpErrorResponse;
  urls = External;
  enableDeploy: boolean;
  blockchainUnresolved: boolean = false;
  blockchainType: string;
  welcomeFragmentExists: boolean = false;
  isOnPrem: boolean;

  alertSub: Subscription;
  routeParamsSub: Subscription;
  routerFragmentChange: Subscription;
  orgProps: OrgProperties;
  routePaths = mainRoutes;

  // Blockchain Service is resolved in the router before loading
  get selectedConsortium(): string {
    return this.blockchainService.blockchainId;
  }

  set selectedConsortium(id: string) {
    if (this.blockchainService.blockchainId === id) { return; }
    let selectObs;
    selectObs = this.blockchainService.select(id).subscribe(selected => {
      if (selected) { this.routeService.outletEnabled = selected; }
      if (selectObs) { selectObs.unsubscribe(); }
    });
  }

  get blockchains(): BlockchainResponse[] {
    return this.blockchainService.blockchains || [];
  }

  // remove #welcome fragment on modal close.
  set welcomeModalOpened(v) { if (!v && this.welcomeFragmentExists) { this.router.navigate([]); } }
  get welcomeModalOpened() { return this.welcomeModal._open; }

  constructor(
    public zone: NgZone,
    public routeService: RouteService,
    private authenticationService: AuthenticationService,
    private router: Router,
    private route: ActivatedRoute,
    private alertService: ErrorAlertService,
    private tourService: TourService,
    private blockchainService: BlockchainService,
    private personaService: PersonaService,
    private helpService: ContextualHelpService
  ) {
    this.env = environment;
    this.blockchainType = this.blockchainService.type;
    const validPath = this.handlePaths();
    if (!validPath) {
      this.routeService.redirectToDefault();
      this.routeService.outletEnabled = false;
    } else {
      this.routeParamsSub = this.route.params.subscribe(param => this.handleParams(param));
      this.alertSub = this.alertService.notify.subscribe(error => this.addAlert(error));
      if (!environment.csp) { this.setInactivityTimeout(); }
      this.orgProps = this.authenticationService.orgProps;
      this.checkAuthorization();
      this.setPlatform();
    }


  }

  ngOnInit() {
    this.tourService.initialUrl = this.router.url.substr(1);
    this.selectedConsortium = this.route.snapshot.params['consortiumId'];

    this.routerFragmentChange = this.route.fragment
      .subscribe(fragment => this.handleFragment(fragment));

  }

  ngOnDestroy(): void {
    if (this.routerFragmentChange) { this.routerFragmentChange.unsubscribe(); }
    if (this.alertSub) { this.alertSub.unsubscribe(); }

    if (!environment.csp) {
      this.deregisterWindowListeners();
    }
  }

  consortiumChange(): void {
    this.routeService.outletEnabled = false;
    this.navDisabled = false;
    if (this.selectedConsortium) {
      this.router.navigate([`/${this.selectedConsortium}`, 'dashboard'])
        .then(() => {
          // This is to refresh all child components
          this.blockchainType = this.blockchainService.type;
          this.routeService.reloadOutlet();
        });
    }
  }

  deployDapp() {
    this.openDeployDapp = true;
  }

  private handlePaths(): boolean {
    let path = decodeURI(this.router.url);
    if (path.indexOf('#') >= 0) { path = path.substr(0, path.indexOf('#')); } // remove fragment part
    if (path.indexOf('?') >= 0) { path = path.substr(0, path.indexOf('?')); } // remove param part
    const paths = path.split('/'); paths.shift();
    if (!paths[0]) {
      return false;
    } else if (paths[0] === mainRoutes.blockchain) { // path is /blockchain*
      this.blockchainUnresolved = true;
      if (mainRoutes.blockchainChildren.indexOf(paths[1]) === -1) {
        return false;
      }
    } else if (uuidRegExp.test(paths[0])) { // valid :consortiumId
      if (mainRoutes.consortiumIdChildren.indexOf(paths[1]) === -1) {
        return false;
      }
    }
    return true;
  }

  private handleParams(param: Params): void {
    const blockchainId = param.consortiumId as string;

    // valid uuidv4 resource string
    if (blockchainId && uuidRegExp.test(blockchainId)) {
      this.selectedConsortium = blockchainId;
      this.navDisabled = false;
      this.sidemenuVisible = true;
      this.routeService.outletEnabled = true;

      // starts with /blockchain/: {welcome, deploy, deploying}
    } else if (this.blockchainUnresolved) {
      this.navDisabled = true;
      this.sidemenuVisible = true;
      this.routeService.outletEnabled = true;
      if (this.blockchainService.noConsortiumJoined) {
        // no consortium joined and was deploying?
        // Resume 'deploying view' since nothing else to show.
        this.routeService.resumeUnfinishedDeployIfExists();
      }

      // catch everything else (including 'undefined', 'login-return')
    } else {
      this.navDisabled = false;
      this.sidemenuVisible = false;
      this.routeService.outletEnabled = false;
      this.routeService.redirectToDefault();
    }
  }

  private handleFragment(fragment: string): void {
    switch (fragment) {
      case 'welcome':
        this.welcomeFragmentExists = true;
        this.welcomeModal.open();
        break;
      default:
        this.welcomeModal.close();
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

  private checkAuthorization(): boolean {
    const blockchainCount = this.blockchains.length;
    const maxChain = this.orgProps.max_chains;
    // Must be a system admin or consortium admin to deploy
    if (!(this.personaService.hasAuthorization(Personas.SystemsAdmin)
      || this.personaService.hasAuthorization(Personas.ConsortiumAdmin))) {
      this.enableDeploy = false;

      return this.enableDeploy;
    }


    this.enableDeploy = (maxChain === 0) || (maxChain > blockchainCount);

    return this.enableDeploy;
  }

  private setPlatform(): void {
    if (this.blockchainService.zones) {
      this.isOnPrem = this.blockchainService.zones.some((zone) => zone.type === ZoneType.ON_PREM);
    }
  }

  onClickToHelp() {
    this.helpService.openHelpHome();
  }

}
