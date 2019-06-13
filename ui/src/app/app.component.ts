/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { environment } from './../environments/environment';
import { Component, OnInit } from '@angular/core';
import { Title } from '@angular/platform-browser';

import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'concord-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnInit {

  constructor(
    private titleService: Title,
    private translateService: TranslateService,
  ) {}

  ngOnInit() {
    this.setTitle();
    if (environment.csp) {
      if (window.location.search.indexOf('org_link') !== -1) {
        window.location.href = `https://${window.location.host}/api/oauth/login${window.location.search}`;
      } else if (window.location.pathname === '/') {
        window.location.href = `${window.location.pathname}/dashboard`;
      }
    }
  }

  private setTitle() {
    this.titleService.setTitle(
      this.translateService.instant('title')
    );
  }

}
