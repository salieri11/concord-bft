/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { PersonaService } from './persona.service';

describe('PersonaService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [PersonaService]
    });
  });

  it('should be created', inject([PersonaService], (service: PersonaService) => {
    expect(service).toBeTruthy();
  }));
});
