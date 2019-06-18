/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */


import { Component, OnInit } from '@angular/core';
import { AuthenticationService } from '../shared/authentication.service';
import { BlockchainService } from '../shared/blockchain.service';
import { Router } from '../../../node_modules/@angular/router';

import * as Vivus from 'vivus';

import { environment } from './../../environments/environment';

@Component({
  selector: 'concord-marketing',
  templateUrl: './marketing.component.html',
  styleUrls: ['./marketing.component.scss']
})
export class MarketingComponent implements OnInit {
  logo: Vivus;
  onGoing: Vivus;
  constructor(
    private authenticationService: AuthenticationService,
    private router: Router,
    private blockchainService: BlockchainService,
  ) {
    if (environment.csp) {
      if (window.location.search.indexOf('org_link') !== -1) {
        window.location.href = `https://${window.location.host}/api/oauth/login${window.location.search}`;
      } else if (window.location.pathname === '/') {
        window.location.href = `${window.location.pathname}/dashboard`;
      }
    }
  }

  ngOnInit() {
    this.initLogo();

    if (this.authenticationService.isAuthenticated()) {
      this.blockchainService.set().subscribe(() => {
        const blockchain = this.blockchainService.blockchainId;
        this.router.navigate([`/${blockchain}`, 'dashboard']);
      });
    }
  }

  initLogo(): void {
    this.logo = new Vivus('logo', {
      type: 'sync',
      duration: 250
    });
  }

}
