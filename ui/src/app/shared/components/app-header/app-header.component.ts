/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnDestroy, ViewChild, AfterViewInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { ClrDropdown } from '@clr/angular';
import { TranslateService } from '@ngx-translate/core';
import {
  CspApiService,
  CspHeaderTheme,
  CspHeaderOptions,
  CspEnvironment,
} from '@vmw/csp-ngx-components';
import { VmwClarityThemeService } from '../../theme.provider';

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

  constructor(
    private authenticationService: AuthenticationService,
    private personaService: PersonaService,
    private tourService: TourService,
    private cspApiService: CspApiService,
    private translateService: TranslateService,
    private themeService: VmwClarityThemeService,
  ) {
    console.log(this.env.cspEnv)
    if (this.env.csp) {
      if (this.env.cspEnv === 'staging') {
        this.cspApiService.setCspEnvironment(CspEnvironment.STAGING);
      } else if (this.env.cspEnv === 'production') {
        this.cspApiService.setCspEnvironment(CspEnvironment.PRODUCTION);
      }
      this.setupCSP();
      // Init theme for csp
      this.setTheme();
      // Update theme based on theme changes
      this.themeService.themeChange
        .subscribe(() => this.setTheme());

        this.authToken = this.authenticationService.accessToken;

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
        console.log('org', org);
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
    this.headerOptions.docsProducts = ['Blockchain'];
    this.headerOptions.docsDefaultSearch = 'Blockchain';
    this.headerOptions.userMenuIconified = true;
    this.headerOptions.notifications = null;
    this.headerOptions.alerts = null;
    this.headerOptions.context = null;
    this.headerOptions.showOrgSwitcher = true;
    this.headerOptions.showHelpMenu = true;
    this.headerOptions.enableChangeDefaultOrg = true;
    this.headerOptions.globalBranding = true;
    this.headerOptions.isMasked = false;
    this.headerOptions.showSupportTab = true;
    this.headerOptions.showDocCenterButton = true;
  }

  private setTheme() {
    const theme = this.themeService.theme.toUpperCase();
    this.headerOptions.theme = CspHeaderTheme[theme];
  }

}
