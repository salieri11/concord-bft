import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ConsortiumService } from './consortium.service';

describe('ConsortiumService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
    	imports: [HttpClientTestingModule],
      providers: [ConsortiumService]
    });
  });

  it('should be created', inject([ConsortiumService], (service: ConsortiumService) => {
    expect(service).toBeTruthy();
  }));
});
