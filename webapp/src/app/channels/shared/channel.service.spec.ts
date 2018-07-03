/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { ChannelService } from './channel.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('ChannelService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        ChannelService,
        {provide: ANDES_API_PREFIX, useValue: '/api'},
      ]
    });
  });

  it('should be created', inject([ChannelService], (service: ChannelService) => {
    expect(service).toBeTruthy();
  }));
});
