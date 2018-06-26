/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';

@Injectable()
export class AuthenticationService {
  private userSubject: BehaviorSubject<string>;
  readonly user: Observable<string>;

  constructor() {
    this.userSubject = new BehaviorSubject<string>(localStorage['helen.email']);
    this.user = this.userSubject.asObservable();
  }

  isAuthenticated() {
    return localStorage['helen.email'] !== undefined;
  }

  logIn(email: string, password: string) {
    password = '';
    localStorage.setItem('helen.email', email);
    localStorage.setItem('helen.password', password);
    this.userSubject.next(email);
  }

  logOut() {
    localStorage.removeItem('helen.email');
    this.userSubject.next(undefined);
  }
}
