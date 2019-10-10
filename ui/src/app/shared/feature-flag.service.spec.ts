/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FeatureFlagService } from './feature-flag.service';

fdescribe('Service: FeatureFlag', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [FeatureFlagService]
    });
  });

  fit('should ...', inject([FeatureFlagService], (service: FeatureFlagService) => {

    expect(service).toBeTruthy();

  }));


});
