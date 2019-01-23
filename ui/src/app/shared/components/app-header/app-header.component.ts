/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { ClrDropdown } from '@clr/angular';
import { TranslateService } from '@ngx-translate/core';
import { CspApiService, CspHeaderTheme, CspHeaderOptions } from '@vmw/csp-ngx-components';
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
export class AppHeaderComponent implements OnDestroy, OnInit {
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
    if (this.env.csp) {
      this.cspApiService.enableProxy(true);
      this.connectToCsp();
      // Init theme for csp
      this.setTheme();
      // Update theme based on theme changes
      this.themeService.themeChange
        .subscribe(() => this.setTheme());
    }

    this.authenticationChange = authenticationService.user.subscribe(user => {
      this.username = user.email;
      this.personaService.currentPersona = user.persona;
    });

    this.userProfileMenuToggleChanges = this.tourService.userProfileDropdownChanges$.subscribe((openMenu) => {
      setTimeout(() => {
        this.userProfileMenu.ifOpenService.open = openMenu;
      });
    });
  }

  ngOnInit() {
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onLogOut() {
    this.authenticationService.logOut();
  }

  onPersonaChange(persona: Personas) {
    localStorage.setItem('helen.persona', persona);
    this.personaService.currentPersona = persona;
    location.reload();
  }

  private connectToCsp() {
    // TODO: Most of this is fake data until we can get
    // these services setup.
    this.serviceRefLink = 'http://blockchain-stage.vmware.com';

    /* tslint:disable */
    this.authToken = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InNpZ25pbmdfMiJ9.eyJzdWIiOiJ2bXdhcmUuY29tOjY0ZTc1MGM2LWMxMTYtNGRjMy1iN2VjLWU3MjI0NmFiM2YxOSIsImF6cCI6ImNzcF9zdGdfZ2F6X2ludGVybmFsX2NsaWVudF9pZCIsImRvbWFpbiI6InZtd2FyZS5jb20iLCJjb250ZXh0IjoiMDdkZTkwMTQtNGUzMy00YWUxLWIxNjQtYzRhZTFmNjUxNTJiIiwiaXNzIjoiaHR0cHM6Ly9nYXotcHJldmlldy5jc3AtdmlkbS1wcm9kLmNvbSIsInBlcm1zIjpbImNzcDpvcmdfb3duZXIiLCJjc3A6c2VydmljZV9vd25lciJdLCJjb250ZXh0X25hbWUiOiI5MGVhZTA0Zi1hZGE3LTRiYTMtYmRmYy0yMDBjMzY0MTM0NzMiLCJleHAiOjE1NDc1OTI2NTUsImlhdCI6MTU0NzU5MDg1NSwianRpIjoiYTAyY2Y4YjctZjA0Mi00ZWVmLWIxYmUtMTFlNjhhZWFiODkzIiwiYWNjdCI6Im1oYXJyaXNvbkB2bXdhcmUuY29tIiwidXNlcm5hbWUiOiJtaGFycmlzb24ifQ.R91xkT0TkNFTOyeaDOqXjSxG5xjkzqisQOCQBo29uHNWSmtHXwyzbgEIXnBqswtmDsTv_hro1IsiVmdzX9MWaGArxb0IrtCbHAwu--5dy-S6DKfxzuTt1DvvL_NMiJ_vdbTTP0G-tQs1hkpX2ULx78iO7sfEjGYG_MUbRFaTcS4rXiOLer288OJ4gMCRqm6uj1WsCSW13or91O99xE9h2LTtfyrQWrWPAds6zH4TFrSEKhTo32OnEyuHk7-LZisLcQvxKktzBnBKPtsvR2ecR-2KzmvQJgVBnvsfdlOU6Dm8Wvyme87dWyW8EX9gdifKpZujWUoeb_zBkffeIPs3gw';
    /* tslint:enable */

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
    this.headerOptions.docCenterLink = 'http//docs.vmware.com/blockchain';
    this.headerOptions.statusIOComponentId = '12235324';
    this.headerOptions.suppressCookieAlert = true;
    this.headerOptions.docsProducts = ['Blockchain'];
    this.headerOptions.docsDefaultSearch = 'Blockchain';
    this.headerOptions.userMenuIconified = true;
    this.headerOptions.notifications = null;
    this.headerOptions.alerts = null;
    this.headerOptions.context = null;
    this.headerOptions.showOrgSwitcher = true;
    this.headerOptions.showHelpMenu = true;
    this.headerOptions.enableChangeDefaultOrg = true;
    this.headerOptions.enableEditProfileLink = true;
    this.headerOptions.showUserSettingsSection = true;
    this.headerOptions.globalBranding = true;
    this.headerOptions.isMasked = false;
    this.headerOptions.showSupportTab = true;
    this.headerOptions.showDocCenterButton = true;
    this.headerOptions.communitiesLink = 'http://nowhere';
  }

  private setTheme() {
    const theme = this.themeService.theme.toUpperCase();
    this.headerOptions.theme = CspHeaderTheme[theme];
  }

}
