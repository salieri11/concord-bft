/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild, OnDestroy } from '@angular/core';
import { Location } from '@angular/common';
import { BlockchainWizardComponent } from '../../blockchain/blockchain-wizard/blockchain-wizard.component';
import { Subscription } from 'rxjs';
import { TranslateService } from '@ngx-translate/core';
import { RouteService } from '../../shared/route.service';

@Component({
  selector: 'concord-deploy',
  templateUrl: './deploy.component.html',
  styleUrls: ['./deploy.component.scss']
})
export class DeployComponent implements OnInit, OnDestroy {

  @ViewChild('blockchainWizard', { static: true }) blockchainWizard: BlockchainWizardComponent;

  wizardEventsSub: Subscription;

  constructor(
    private location: Location,
    private translateService: TranslateService,
    private routeService: RouteService,
    ) { }

  translate(a: string) { return this.translateService.instant(a); }

  ngOnInit() {
    this.wizardEventsSub = this.blockchainWizard.events.subscribe((e) => {
      switch (e.type) {
        case 'cancel':
          this.location.back();
          /**
           * ? Possible Edge Cases
           *
           * if the user **directly lands** on this deploy page through temp redirects
           * such as `login-return`, location.back() will fail, and might lead to some
           * behavior that redirects users away from this website (e.g. back to New Tab page)
           *
           * However, this is only possible when user gets to this page
           * by a direct link, such as https://localhost.vmware.com/blockchain/deploy
           * or `login-return` itself redirects to this page
           *
           * Update: RouteService offers `goToPreviousPage` which safely navigates
           * back to previous path with handling these edge cases and fleeting routes
           */
          break;
        case 'deployError':
          this.routeService.redirectToDefault();
          break;
      }
    });
    this.blockchainWizard.open();
  }

  ngOnDestroy () {
    if (this.wizardEventsSub) { this.wizardEventsSub.unsubscribe(); }
  }

}
