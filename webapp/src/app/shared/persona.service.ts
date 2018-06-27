/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';

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

  constructor() {}

  get currentPersona() {
    return this._currentPersona;
  }

  set currentPersona(persona: string) {
    this._currentPersona = persona;
    localStorage.setItem('helen.persona', persona);
  }

  public hasAuthorization(roles: any): boolean {
    roles = Array.isArray(roles) ? roles : [roles];
    return roles.indexOf(this.currentPersona) !== -1;
  }
}
