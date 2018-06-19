/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { CanActivate, CanActivateChild, Router } from '@angular/router';

import { AuthenticationService } from './authentication.service';

@Injectable()
export class AuthenticatedGuard implements CanActivateChild, CanActivate {
  constructor(private router: Router, private authenticationService: AuthenticationService) {}

  canActivateChild(): boolean {
    if (!this.authenticationService.isAuthenticated()) {
      this.router.navigate(['auth', 'log-in']);
      return false;
    } else {
      return true;
    }
  }

  canActivate(): boolean {
    if (!this.authenticationService.isAuthenticated()) {
      this.router.navigate(['auth', 'log-in']);
      return false;
    } else {
      return true;
    }
  }
}

