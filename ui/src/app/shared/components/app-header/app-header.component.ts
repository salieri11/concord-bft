/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ViewChild, AfterViewInit } from '@angular/core';
import { Subscription, Subject } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import {
  CspApiService,
  CspHeaderOptions,
  CspEnvironment,
  CspHeaderAppAlertConfig
} from '@vmw/csp-ngx-components';

import { environment } from '../../../../environments/environment';
import { AuthenticationService } from '../../authentication.service';
import { Personas, PersonaService } from '../../persona.service';
import { CspAPIs } from '../../../shared/urls.model';
import { ErrorAlertService } from '../../global-error-handler.service';

@Component({
  selector: 'concord-app-header',
  templateUrl: './app-header.component.html',
  styleUrls: ['./app-header.component.scss']
})
export class AppHeaderComponent implements AfterViewInit {
  @ViewChild('header', { static: false }) header: any;
  authenticationChange: Subscription;
  userProfileMenuToggleChanges: Subscription;
  personas = Personas;
  personaOptions = PersonaService.getOptions();
  authenticated = false;
  username: string;
  authToken: string;
  headerOptions: CspHeaderOptions = new CspHeaderOptions();
  alertStream: Subject<CspHeaderAppAlertConfig>;

  serviceRefLink: string;
  env: any = environment;
  cspEnvironment: CspEnvironment;

  constructor(
    private authenticationService: AuthenticationService,
    private cspApiService: CspApiService,
    private translateService: TranslateService,
    private alertService: ErrorAlertService
  ) {
    if (this.env.csp) {
      this.alertStream = this.alertService.cspAlertStream;
      this.setupCSP();
    }
  }

  ngAfterViewInit() {
    if (this.env.csp) {
      this.header.switchOrg.subscribe(org => {
        const orgLink = `${CspAPIs.orgs}${org.id}`;
        window.location.href = `${this.env.loginPath}?org_link=${orgLink}&orgLink=${orgLink}`;
      });

      this.header.signOut.subscribe(() => {
        window.location.href = this.authenticationService.logoutPath;
      });

      this.header.tokenRefreshNeeded.subscribe(() => this.authenticationService.saveLastLocationAndReAuth());
    }
  }

  private setupCSP() {
    this.authToken = this.authenticationService.accessToken;
    this.cspApiService.authToken = this.authToken;
    if (this.env.cspEnv === 'staging') {
      this.cspApiService.setCspEnvironment(CspEnvironment.STAGING);
      this.cspEnvironment = CspEnvironment.STAGING;
    } else if (this.env.cspEnv === 'production') {
      this.cspApiService.setCspEnvironment(CspEnvironment.PRODUCTION);
      this.cspEnvironment = CspEnvironment.PRODUCTION;
    }

    // TODO: Most of this is fake data until we can get
    // these services setup.
    this.serviceRefLink = this.env.refLink;

    // HeaderOptions
    this.headerOptions.baseRoute = '/';
    this.headerOptions.bugRoute = '/';
    this.headerOptions.title = this.translateService.instant('title');
    this.headerOptions.subTitle = this.translateService.instant('subTitle');
    this.headerOptions.showBackdrop = true;
    this.headerOptions.showSignIn = true;
    this.headerOptions.enableSignout = true;
    this.headerOptions.showNotificationsMenu = true;
    this.headerOptions.helpPinnable = true;
    this.headerOptions.disableDocsSearch = true;
    this.headerOptions.userMenuIconified = true;
    this.headerOptions.notifications = null;
    this.headerOptions.alerts = null;
    this.headerOptions.context = null;
    this.headerOptions.showOrgSwitcher = true;
    this.headerOptions.showHelpMenu = false;
    this.headerOptions.enableChangeDefaultOrg = true;
    this.headerOptions.enableEventTracking = true;
    this.headerOptions.enableIntercom = false;
    this.headerOptions.globalBranding = true;
    this.headerOptions.isMasked = false;
    this.headerOptions.showSupportTab = false;
    this.headerOptions.showDocCenterButton = false;
  }

}
