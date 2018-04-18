/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {
  title = 'app';

  constructor (translate: TranslateService) {
    const browserLang = translate.getBrowserLang();

    translate.setDefaultLang('en');
    translate.use(browserLang);
  }
}
