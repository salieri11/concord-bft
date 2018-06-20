/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, NgZone } from '@angular/core';
import { Subscription } from 'rxjs/Subscription';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  title = 'app';
  alerts: any = [];
  authenticationChange: Subscription;

  authenticated = false;
  username: string;

  constructor(
    private translate: TranslateService,
    public zone: NgZone,
  ) {
    const browserLang = this.translate.getBrowserLang();
    this.translate.setDefaultLang('en');
    this.translate.use(browserLang);
  }
}
