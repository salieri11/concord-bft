/*
 * Copyright 2018 VMware, all rights reserved.
 */


import { Component, OnInit } from '@angular/core';

import * as Vivus from 'vivus';

@Component({
  selector: 'athena-marketing',
  templateUrl: './marketing.component.html',
  styleUrls: ['./marketing.component.scss']
})
export class MarketingComponent implements OnInit {
  logo: Vivus;
  onGoing: Vivus;

  constructor() {}

  ngOnInit() {
    this.initLogo();
  }

  initLogo(): void {
    this.logo = new Vivus('logo', {
      type: 'sync',
      duration: 250
    });
  }

}
