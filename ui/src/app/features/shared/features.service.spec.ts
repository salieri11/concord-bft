/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FeaturesService } from './features.service';

describe('FeaturesService', () => {
  beforeEach(() => TestBed.configureTestingModule({
    imports: [
      HttpClientTestingModule,
    ]
  }));

  it('should be created', () => {
    const service: FeaturesService = TestBed.get(FeaturesService);
    expect(service).toBeTruthy();
  });
});
