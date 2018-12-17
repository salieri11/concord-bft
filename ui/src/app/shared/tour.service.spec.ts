/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TranslateService } from '@ngx-translate/core';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';

import { TourService } from './tour.service';
import { PersonaService } from './persona.service';
import { MockTranslateService } from '../mocks/mock-translate.module';

describe('TourService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule],
      providers: [
        TourService,
        NgxTourService,
        PersonaService,
        {provide: TranslateService, useClass: MockTranslateService}
      ]
    });
  });

  it('should be created', inject([TourService], (service: TourService) => {
    expect(service).toBeTruthy();
  }));
});
