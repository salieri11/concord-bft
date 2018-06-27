/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';

import { Personas, PersonaService } from './persona.service';

export interface User {
  email: string;
  persona: Personas;
}

@Injectable()
export class AuthenticationService {
  private userSubject: BehaviorSubject<User>;
  readonly user: Observable<User>;

  constructor(private personaService: PersonaService) {
    this.userSubject = new BehaviorSubject<User>({email: localStorage['helen.email'], persona: localStorage['helen.persona']});
    this.user = this.userSubject.asObservable();
  }

  isAuthenticated() {
    return localStorage['helen.email'] !== undefined;
  }

  logIn(email: string, password: string, persona: Personas) {
    password = '';
    localStorage.setItem('helen.email', email);
    localStorage.setItem('helen.password', password);
    this.personaService.currentPersona = persona;
    this.userSubject.next({
      email: email,
      persona: persona
    });
  }

  logOut() {
    localStorage.removeItem('helen.email');
    localStorage.removeItem('helen.persona');
    this.personaService.currentPersona = undefined;
    this.userSubject.next({email: localStorage['helen.email'], persona: localStorage['helen.persona']});
  }
}
