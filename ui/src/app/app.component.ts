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
    }
  }

  private setTitle() {
    this.titleService.setTitle(
      this.translateService.instant('title')
    );
  }

}
