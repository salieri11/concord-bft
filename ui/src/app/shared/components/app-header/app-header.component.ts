/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, ViewChild, AfterViewInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { ClrDropdown } from '@clr/angular';
import { TranslateService } from '@ngx-translate/core';
import {
  CspApiService,
  CspHeaderOptions,
  CspEnvironment,
} from '@vmw/csp-ngx-components';

import { environment } from '../../../../environments/environment';
import { AuthenticationService } from '../../authentication.service';
import { Personas, PersonaService } from '../../persona.service';
import { TourService } from '../../tour.service';

@Component({
  selector: 'concord-app-header',
  templateUrl: './app-header.component.html',
  styleUrls: ['./app-header.component.scss']
})
export class AppHeaderComponent implements OnDestroy, AfterViewInit {
  @ViewChild('header') header: any;
  @ViewChild('userProfileMenu') userProfileMenu: ClrDropdown;
  authenticationChange: Subscription;
  userProfileMenuToggleChanges: Subscription;
  personas = Personas;
  personaOptions = PersonaService.getOptions();
  authenticated = false;
  username: string;
  authToken: string;
  headerOptions: CspHeaderOptions = new CspHeaderOptions();

  serviceRefLink: string;
  env: any = environment;
  cspEnvironment: CspEnvironment;

  constructor(
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private tourService: TourService,
    private cspApiService: CspApiService,
    private translateService: TranslateService,
  ) {
    if (this.env.csp) {
      this.setupCSP();
    } else {
      this.userProfileMenuToggleChanges = this.tourService.userProfileDropdownChanges$.subscribe((openMenu) => {
        setTimeout(() => {
          this.userProfileMenu.ifOpenService.open = openMenu;
        });
      });

      this.authenticationChange = authenticationService.user.subscribe(user => {
        this.username = user.email;
        this.personaService.currentPersonas.push(user.persona);
      });
    }
  }

  ngAfterViewInit() {
    if (this.env.csp) {
      this.header.switchOrg.subscribe(org => {
        window.location.href = `${this.env.loginPath}?org_link=/csp/gateway/am/api/orgs/${org.id}&orgLink=/csp/gateway/am/api/orgs/${org.id}`;
      });

      this.header.signOut.subscribe(() => {
        window.location.href = this.authenticationService.logoutPath;
      });
    }
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onLogOut() {
    this.authenticationService.logOut();
  }

  onPersonaChange(persona: Personas) {
    localStorage.setItem('helen.persona', persona);
    this.personaService.currentPersonas.push(persona);
    location.reload();
  }

  private setupCSP() {
    if (this.env.cspEnv === 'staging') {
      this.cspApiService.setCspEnvironment(CspEnvironment.STAGING);
      this.cspEnvironment = CspEnvironment.STAGING;
    } else if (this.env.cspEnv === 'production') {
      this.cspApiService.setCspEnvironment(CspEnvironment.PRODUCTION);
      this.cspEnvironment = CspEnvironment.PRODUCTION;
    }
    this.authToken = this.authenticationService.accessToken;

    // TODO: Most of this is fake data until we can get
    // these services setup.
    this.serviceRefLink = 'https://blockchain-stage.vmware.com';

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
    this.headerOptions.docCenterLink = 'https://docs-staging.vmware.com/en/VMware-Blockchain/index.html';
    this.headerOptions.docsProducts = ['VMware Blockchain'];
    this.headerOptions.docsDefaultSearch = 'Blockchain';
    this.headerOptions.userMenuIconified = true;
    this.headerOptions.notifications = null;
    this.headerOptions.alerts = null;
    this.headerOptions.context = null;
    this.headerOptions.showOrgSwitcher = true;
    this.headerOptions.showHelpMenu = true;
    this.headerOptions.enableChangeDefaultOrg = true;
    this.headerOptions.enableEventTracking = true;
    this.headerOptions.enableIntercom = true;
    this.headerOptions.globalBranding = true;
    this.headerOptions.isMasked = false;
    this.headerOptions.showSupportTab = true;
    this.headerOptions.showDocCenterButton = true;
  }

}
