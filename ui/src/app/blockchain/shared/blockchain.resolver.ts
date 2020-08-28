/*
 * Copyright 2020 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { Resolve, ActivatedRouteSnapshot } from '@angular/router';
import { Observable, of, zip } from 'rxjs';
import { catchError } from 'rxjs/operators';

import { BlockchainService } from './blockchain.service';
import { NodesService } from '../../nodes/shared/nodes.service';

import { uuidRegExpLax, mainRoutes } from '../../shared/urls.model';


@Injectable({
  providedIn: 'root'
})
export class BlockchainResolver implements Resolve<boolean> {
  constructor(
    private router: Router,
    private blockchainService: BlockchainService,
    private nodesService: NodesService,
  ) { }
  resolve(route?: ActivatedRouteSnapshot): Observable<boolean | any> {
    const blockchainId = route ? route.params['blockchainId'] : null;
    const catchErrorHandler = catchError(error => {
      this.router.navigate([`/${mainRoutes.error}`],
      { queryParams: { error: JSON.stringify(error) } });
      return error;
    });
    if (uuidRegExpLax.test(blockchainId)) { // UUID Given
      return zip(
        this.blockchainService.set(blockchainId),
        this.nodesService.fetchAndSet(blockchainId),
      ).pipe(catchErrorHandler);
    } else { // route without :blockchainId
      return this.blockchainService.set().pipe(catchErrorHandler);
    }
  }

}


export class MockBlockchainResolver implements Resolve<boolean> {
  fallbackTo?: string;
  resolve(): Observable<boolean | any> {
    return of(true);
  }
}
