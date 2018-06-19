/*
 * Copyright 2018 VMware, all rights reserved.
 */
import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { KubernetesService } from './kubernetes.service';

describe('KubernetesService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [KubernetesService]
    });
  });

  it('should be created', inject([KubernetesService], (service: KubernetesService) => {
    expect(service).toBeTruthy();
  }));
});
