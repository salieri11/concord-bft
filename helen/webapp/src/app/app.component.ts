/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { Title } from '@angular/platform-browser';

import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'athena-root',
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
  }

  private setTitle() {
    this.titleService.setTitle(
      this.translateService.instant('title')
    );
  }

}
