/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Router } from '@angular/router';

import { RouteService } from './route.service';
import { testFor, beforeTesting, prepareEach } from '../../test.helper.spec';
import { mainRoutes } from './urls.model';

describe('RouteService', () => {
  let router: Router;
  let routeService: RouteService;
  const defaultRoute = [ `/${mainRoutes.blockchain}/${mainRoutes.welcome}` ];
  const redirectTo = '/84391b6e-e24d-4c8e-a8ae-09826cc15d7d/nodes/committers';

  const test = testFor(RouteService).expedite({
    imports: [], provides: [], declarations: [],
  }, beforeTesting(() => {
    routeService = test.component;
    router = test.getService(Router);
  }), prepareEach(() => {}));

  it('should be created', () => {
    expect(routeService).toBeTruthy();
  });

  it('should be redirected to last location', () => {
    localStorage.setItem('lastLocation', `${new Date()}--${redirectTo}`);
    const navigateSpy = spyOn(router, 'navigateByUrl');

    routeService.loginReturnHandler();
    expect(navigateSpy).toHaveBeenCalledWith(redirectTo);
  });

  it('should not be redirected to last location and redirected to default', () => {
    const threeMinutesAgo = new Date( Date.now() - 1000 * 60 * 3);
    localStorage.setItem('lastLocation', `${threeMinutesAgo}--${redirectTo}`);
    const navigateSpy = spyOn(router, 'navigate');

    routeService.loginReturnHandler();

    expect(navigateSpy).not.toHaveBeenCalledWith(redirectTo);
    expect(navigateSpy).toHaveBeenCalledWith(defaultRoute);
  });

});
