/*
 * Copyright 2018 VMware, all rights reserved.
 */
import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { KubernetesService } from './kubernetes.service';
import { ANDES_API_PREFIX } from '../../shared/shared.config';

describe('KubernetesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [
        KubernetesService,
        {provide: ANDES_API_PREFIX, useValue: 'api'},
      ]
    });
  });

  it('should be created', inject([KubernetesService], (service: KubernetesService) => {
    expect(service).toBeTruthy();
  }));
});
