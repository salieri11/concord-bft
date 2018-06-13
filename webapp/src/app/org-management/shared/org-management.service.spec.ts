import { TestBed, inject } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { OrgManagementService } from './org-management.service';

describe('OrgManagementService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
    	imports: [HttpClientTestingModule],
      providers: [OrgManagementService]
    });
  });

  it('should be created', inject([OrgManagementService], (service: OrgManagementService) => {
    expect(service).toBeTruthy();
  }));
});
