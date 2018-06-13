import { TestBed, inject } from '@angular/core/testing';

import { HttpClient, HttpHandler } from '@angular/common/http';

import { BlockchainsService } from './blockchains.service';

describe('BlockchainsService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [BlockchainsService, HttpClient, HttpHandler]
    });
  });

  it('should be created', inject([BlockchainsService], (service: BlockchainsService) => {
    expect(service).toBeTruthy();
  }));
});
