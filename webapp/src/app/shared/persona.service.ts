/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Subject } from 'rxjs/Subject';

export enum Personas {
  SystemsAdmin = 'systems_admin',
  ConsortiumAdmin = 'consortium_admin',
  OrgAdmin = 'org_admin',
  OrgDeveloper = 'org_developer',
  OrgUser = 'org_user'

}

@Injectable()
export class PersonaService {

  private _currentPersona: string;
  private personaChangedSource: Subject<void> = new Subject<void>();
  personaChanged$ = this.personaChangedSource.asObservable();

  constructor() {}

  get currentPersona() {
    return this._currentPersona;
  }

  set currentPersona(persona: string) {
    this._currentPersona = persona;
    localStorage.setItem('helen.persona', persona);
    this.personaChangedSource.next();
  }

  public hasAuthorization(roles: any): boolean {
    roles = Array.isArray(roles) ? roles : [roles];
    return roles.indexOf(this.currentPersona) !== -1;
  }
}
