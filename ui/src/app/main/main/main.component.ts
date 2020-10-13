/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, NgZone, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';
import { HttpErrorResponse } from '@angular/common/http';
import { Subscription } from 'rxjs';

import { environment } from '../../../environments/environment';
import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { Personas, PersonaService } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';

import { BlockchainResponse } from '../../blockchain/shared/blockchain.model';
import { External, mainRoutes, uuidRegExp, mainFragments } from '../../shared/urls.model';
import { OrgProperties } from '../../orgs/shared/org.model';

import { ClrModal } from '@clr/angular';
import { RouteService } from '../../shared/route.service';
import { ContextualHelpService } from './../../shared/contextual-help.service';
import { NodesService } from '../../nodes/shared/nodes.service';

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
  isOnPrem: boolean = true;

  alertSub: Subscription;
  routeParamsSub: Subscription;
  routerFragmentChange: Subscription;
  orgProps: OrgProperties;
  routePaths = mainRoutes;
  loading: boolean = true;

  private currentCheckedPath: string;

  // Blockchain Service is resolved in the router before loading
  get selectedBlockchainId(): string {
    return this.blockchainService.blockchainId;
  }

  set selectedBlockchainId(id: string) {
    if (this.blockchainService.blockchainId === id) { return; }
    this.blockchainService.select(id).subscribe(() => this.handleUrl());
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
    private nodesService: NodesService,
    private personaService: PersonaService,
    private helpService: ContextualHelpService
  ) {
    this.env = environment;
    this.routeService.outletEnabled = false; // Hide while possible redirects
    this.blockchainType = this.blockchainService.type;
    this.orgProps = this.authenticationService.orgProps;
    this.checkDeployAuthorization();
  }

  ngOnInit() {
    this.tourService.initialUrl = this.router.url.substr(1);
    this.selectedBlockchainId = this.route.snapshot.params['blockchainId'];
    this.routeParamsSub = this.route.url.subscribe(() => this.handleUrl());
    this.routerFragmentChange = this.route.fragment.subscribe(fragment => this.handleFragment(fragment));
    this.alertSub = this.alertService.notify.subscribe(error => this.addAlert(error));
    this.handleUrl();
  }

  ngOnDestroy(): void {
    if (this.routerFragmentChange) { this.routerFragmentChange.unsubscribe(); }
    if (this.routeParamsSub) { this.routeParamsSub.unsubscribe(); }
    if (this.alertSub) { this.alertSub.unsubscribe(); }
  }

  blockchainChange(): void {
    if (this.selectedBlockchainId) {
      this.navDisabled = false;
      this.routeService.outletEnabled = false;
      const subpaths = this.router.url.split('/');
      subpaths.shift(); subpaths.shift(); // remove front slash & blockchain id
      subpaths.unshift('/' + this.selectedBlockchainId); // change to selected blockchain id
      const newPath = subpaths.join('/');
      this.router.navigate([newPath],  {})
        .then(() => {
          // This is to refresh all child components
          this.blockchainType = this.blockchainService.type;
          this.handleUrl();
        });
    }
  }

  deployDapp() {
    this.openDeployDapp = true;
  }

  private handleUrl(): void {
    const paths = this.routeService.parseURLData(this.router.url).paths;
    if (this.currentCheckedPath === this.router.url) { return; } // already checked path
    this.currentCheckedPath = this.router.url;

    const blockchainId = paths[0] as string;
    const modulePath = paths[1] as string; // childRoute (e.g. dashboard, nodes, organizations, etc.)
    const blockchainsList = this.blockchainService.blockchains;
    this.isOnPrem = this.nodesService.allOnPremZones;
    this.blockchainType = this.blockchainService.type;

    // Valid path with good blockchain id that exists on blockchains list and has valid child module path
    let isValidPath = false;
    this.navDisabled = true;
    if (blockchainId && uuidRegExp.test(blockchainId) &&
        blockchainsList.filter(bc => bc.id === blockchainId).length > 0
        && mainRoutes.blockchainIdChildren.indexOf(modulePath) >= 0) {
      // =>  /{:blockchainId}/{dashboard|blocks|nodes|smart-contracts|logging|consortiums|organizations
      //                       |users|transactions|developer|... etc.} // only checking 1st level childRoute
      isValidPath = true;
      this.navDisabled = false;

    // =>  /blockchain/{welcome|zones|deploy|deploying}/* pages
    } else if (blockchainId === mainRoutes.blockchain
                && mainRoutes.blockchainChildren.indexOf(modulePath) >= 0) {
      isValidPath = true;
      this.navDisabled = true;
      if (this.blockchainService.noConsortiumJoined) {
        // no consortium joined and was deploying?
        // Resume 'deploying view' since nothing else to show.
        this.routeService.resumeUnfinishedDeployIfExists();
      }
    }

    // catch everything else (including 404 blockchain, 'undefined', 'login-return', other crazy paths)
    if (!isValidPath) {
      this.navDisabled = true;
      this.routeService.redirectToDefault();
      return;
    }
    this.routeService.outletEnabled = true; // Everything checked out; show.
  }

  private handleFragment(fragment: string): void {
    switch (fragment) {
      case mainFragments.welcome:
        this.welcomeFragmentExists = true;
        this.welcomeModal.open();
        break;
      default:
        this.welcomeModal.close();
        break;
    }
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

  private checkDeployAuthorization(): boolean {
    const blockchainCount = this.blockchains.length;
    const maxChain = this.orgProps ? this.orgProps.max_chains : null;
    // Must be a system admin or consortium admin to deploy
    if (!(this.personaService.hasAuthorization(Personas.SystemsAdmin)
      || this.personaService.hasAuthorization(Personas.ConsortiumAdmin))) {
      this.enableDeploy = false;

      return this.enableDeploy;
    }

    this.enableDeploy = (maxChain === 0) || (maxChain > blockchainCount);
    this.blockchainService.canDeploy.next(this.enableDeploy);

    return this.enableDeploy;
  }

  onClickToHelp() {
    this.helpService.openHelpHome();
  }

}
