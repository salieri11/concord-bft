/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';

export enum Personas {
  SystemsAdmin = 'SYSTEM_ADMIN',
  ConsortiumAdmin = 'CONSORTIUM_ADMIN',
  OrgAdmin = 'ORG_ADMIN',
  OrgDeveloper = 'ORG_DEVELOPER',
  OrgUser = 'ORG_USER'
}

@Injectable()
export class PersonaService {

  private _currentPersona: Personas;

  constructor() {
  }

  static getOptions(): Array<{ name?: string; value: Personas; }> {
    const personaOptions: Array<{ name?: string; value: Personas; }> = [
      { value: Personas.SystemsAdmin, name: 'personas.systemsAdmin' },
      { value: Personas.ConsortiumAdmin, name: 'personas.consortiumAdmin' },
      { value: Personas.OrgAdmin, name: 'personas.orgAdmin' },
      { value: Personas.OrgDeveloper, name: 'personas.orgDeveloper' },
      { value: Personas.OrgUser, name: 'personas.orgUser' }
    ];
    return personaOptions;
  }

  get currentPersona() {
    return this._currentPersona;
  }

  set currentPersona(persona: Personas) {
    this._currentPersona = persona;
    localStorage.setItem('helen.persona', persona);
  }

  public hasAuthorization(roles: Personas | Personas[]): boolean {
    roles = Array.isArray(roles) ? roles : [roles];
    return roles.indexOf(this.currentPersona) !== -1;
  }
}
