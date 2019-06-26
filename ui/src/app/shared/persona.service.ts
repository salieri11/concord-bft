/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';

export enum Personas {
  SystemsAdmin = 'SYSTEM_ADMIN',
  SystemsInfra = 'SYSTEM_INFRA',
  ConsortiumAdmin = 'CONSORTIUM_ADMIN',
  ConsortiumOperator = 'CONSORTIUM_OPERATOR',
  ConsortiumParticipant = 'CONSORTIUM_PARTICIPANT',
  OrgAdmin = 'ORG_ADMIN',
  OrgDeveloper = 'ORG_DEVELOPER',
  OrgUser = 'ORG_USER'
}


@Injectable()
export class PersonaService {

  currentPersonas: Personas[] = [];

  constructor() {}

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

  static getName(value: Personas): string {
    const matches = PersonaService.getOptions().filter((o) => o.value === value);
    return matches.length ? matches[0].name : null;
  }

  public hasAuthorization(roles: Personas | Personas[]): boolean {
    roles = Array.isArray(roles) ? roles : [roles];
    let isPresent = false;

    roles.forEach(role => {
      if (this.currentPersonas.indexOf(role) !== -1) {
        isPresent = true;
      }
    });
    return isPresent;
  }
}
