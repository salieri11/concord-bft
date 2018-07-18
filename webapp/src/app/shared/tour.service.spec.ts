/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { TourService } from './tour.service';
import { JoyrideModule, JoyrideService } from "ngx-joyride";
import { PersonaService } from "./persona.service";

describe('TourService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [JoyrideModule],
      providers: [TourService, PersonaService, JoyrideService]
    });
  });

  it('should be created', inject([TourService], (service: TourService) => {
    expect(service).toBeTruthy();
  }));
});
