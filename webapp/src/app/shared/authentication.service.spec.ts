/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { TestBed, inject } from '@angular/core/testing';

import { AuthenticationService } from './authentication.service';

describe('AuthenticationService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [AuthenticationService]
    });
  });

  afterEach(inject([AuthenticationService], (service: AuthenticationService) => {
    service.logOut();
  }));

  it('should be created', inject([AuthenticationService], (service: AuthenticationService) => {
    expect(service).toBeTruthy();
  }));

  it('should broadcast an email after log in', inject([AuthenticationService], function(service: AuthenticationService) {
    service.logIn('test@vmware.com', 'asdfasdf');
    const subscription = service.user.subscribe(email => {
      expect(email).toEqual('test@vmware.com');
    });
    subscription.unsubscribe();
  }));

  it('should broadcast undefined after log out', inject([AuthenticationService], function(service: AuthenticationService) {
    service.logOut();
    const subscription = service.user.subscribe(email => {
      expect(email).toBeUndefined();
    });
    subscription.unsubscribe();
  }));
});
