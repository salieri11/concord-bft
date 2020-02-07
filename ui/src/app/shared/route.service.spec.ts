/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';

import { RouteService } from './route.service';
import { getSpecTestingModule } from './shared-testing.module';

describe('RouteService', () => {
  let router: Router;
  let service: RouteService;
  const defaultRoute = [ 'blockchain', 'welcome' ];
  const redirectTo = '/84391b6e-e24d-4c8e-a8ae-09826cc15d7d/nodes/committers';

  beforeEach(async( () => {
    const tester = getSpecTestingModule();

    tester.provideActivatedRoute();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();

    router = TestBed.get(Router);
    service = TestBed.get(RouteService);
  }));

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should be redirected to last location', () => {
    localStorage.setItem('lastLocation', `${new Date()}--${redirectTo}`);
    const navigateSpy = spyOn(router, 'navigateByUrl');

    service.loginReturnHandler();
    expect(navigateSpy).toHaveBeenCalledWith(redirectTo);
  });

  it('should not be redirected to last location and redirected to default', () => {
    const threeMinutesAgo = new Date( Date.now() - 1000 * 60 * 3);
    localStorage.setItem('lastLocation', `${threeMinutesAgo}--${redirectTo}`);
    const navigateSpy = spyOn(router, 'navigate');

    service.loginReturnHandler();

    expect(navigateSpy).not.toHaveBeenCalledWith(redirectTo);
    expect(navigateSpy).toHaveBeenCalledWith(defaultRoute);
  });

});
