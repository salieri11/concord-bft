/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */


import { Component, OnInit } from '@angular/core';
import { AuthenticationService } from '../shared/authentication.service';
import { Router } from '../../../node_modules/@angular/router';

import * as Vivus from 'vivus';

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
    private router: Router
  ) {

  }

  ngOnInit() {
    this.initLogo();
    if (this.authenticationService.isAuthenticated()) {
      this.router.navigate(['dashboard']);
    }
  }

  initLogo(): void {
    this.logo = new Vivus('logo', {
      type: 'sync',
      duration: 250
    });
  }

}
