/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs/Subscription';

import { TranslateService } from '@ngx-translate/core';
import { AuthenticationService } from './shared/authentication.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnDestroy {
  title = 'app';

  authenticationChange: Subscription;

  authenticated = false;
  username: string;

  constructor (private translate: TranslateService,
               private authenticationService: AuthenticationService,
               private router: Router) {
    const browserLang = translate.getBrowserLang();

    translate.setDefaultLang('en');
    translate.use(browserLang);

    this.authenticationChange = authenticationService.user.subscribe(email => {
      this.authenticated = email !== undefined;
      this.username = email;
    });
  }

  ngOnDestroy(): void {
    this.authenticationChange.unsubscribe();
  }

  onLogOut() {
    this.authenticationService.logOut();
    this.router.navigate(['auth', 'log-in']);
  }
}
