/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BlockchainService, BlockchainResolver } from '../blockchain/shared/blockchain.service';
import { Router, NavigationEnd, ActivatedRoute } from '@angular/router';
import { mainRoutes, fleetingRoutesList, ConsortiumStates, uuidRegExp } from './urls.model';
import { DeployStates } from '../blockchain/shared/blockchain.model';

interface RouteHistoryData {
  request?: RouteHistoryData;
  url: string;
  base: string;
  paths: string[];
  params: { [name: string]: string; };
  fragment: string;
}

@Injectable({
  providedIn: 'root'
})
export class RouteService {

  historyData: RouteHistoryData[] = [];
  currentRouteData: RouteHistoryData;
  outletEnabled: Promise<boolean> | boolean = true;

  private initialized: boolean = false;
  private currentQueryParams: any;
  private outputAllRouterEvents = false;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private blockchainService: BlockchainService,
    private blockchainResolver: BlockchainResolver
  ) { this.initialize(); }

  async resolveConsortium() {
    if (this.blockchainService.blockchains) { return; } // already resolved;
    await this.blockchainResolver.resolve().toPromise();
  }

  initialize() {
    if (this.initialized) { return; }
    this.route.queryParams.subscribe(params => {
      this.currentQueryParams = JSON.parse(JSON.stringify(params));
    });
    this.router.events.subscribe(e => {
      if (this.outputAllRouterEvents) { console.log(e); }
      if (e instanceof NavigationEnd) {
        if (this.currentRouteData !== undefined) {
          this.historyData.push(this.currentRouteData);
        }
        this.currentRouteData = this.parseURLData(e.urlAfterRedirects); // resolved destination
        this.currentRouteData.request = this.parseURLData(e.url); // original request URL
      }
    });
    this.initialized = true;
  }

  isPathAlreadyActive(path: string) {
    return this.router.isActive(path, false);
  }

  redirectToDefault(fragment?: string) {
    if (this.blockchainService.blockchains && this.blockchainService.blockchains.length > 0) {
      let consortiumId = this.blockchainService.loadSelectedConsortium();
      if (!consortiumId || this.blockchainService.blockchains.filter(
                            item => item.id === consortiumId).length > 0
        ) {
        consortiumId = this.blockchainService.blockchains[0].id;
      }
      this.router.navigate([consortiumId, mainRoutes.dashboard], {
        fragment: fragment ? fragment : null
      });
    } else { // No consortium joined, redirect to welcome to let user deploy
      this.router.navigate([mainRoutes.blockchain, mainRoutes.welcome]);
    }
  }

  // Reloads router outlet to fresh fetch data and update view as needed
  reloadOutlet() {
    this.outletEnabled = false;
    setTimeout(() => { this.outletEnabled = true; }, 1);
  }

  goToDeploying(taskId: string): boolean {
    if (taskId === ConsortiumStates.waiting || uuidRegExp.test(taskId)) {
      this.router.navigate(mainRoutes.deployingBaseRoute.concat([taskId]));
      return true;
    }
    return false;
  }

  async resumeUnfinishedDeployIfExists(): Promise<boolean> {
    const deployingRegistry = this.blockchainService.loadDeployingData();
    let unfinishedTaskId;
    for (const deployDataKey of Object.keys(deployingRegistry)) {
      const deployData = deployingRegistry[deployDataKey];
      if (Date.now() - deployData.requested > 3600 * 1000) { // Expire old; 1 hour
        delete deployingRegistry[deployDataKey];
        continue;
      }
      if (uuidRegExp.test(deployDataKey)) { // valid uuidv4 taskId, likely interrupted
        try {
          const task = await this.blockchainService.getTask(deployDataKey).toPromise();
          if (!task || task.state === DeployStates.SUCCEEDED) { // invalid or already completed
            delete deployingRegistry[deployDataKey];
            continue;
          } else if (task.state === DeployStates.FAILED) {
            // keep failed for possible retry with exactly the same deploy input
            deployData.state = DeployStates.FAILED;
            continue;
          }
          unfinishedTaskId = deployDataKey;
        } catch (e) { console.log(e); }
      }
    }
    this.blockchainService.saveDeployingData(deployingRegistry, true);

    if (unfinishedTaskId) {
      // go to /blockchain/deploying/{taskId}
      this.goToDeploying(unfinishedTaskId);
      return true;
    } else { return false; }

  }

  getPreviousRouteData() {
    if (this.historyData.length === 0) { return null; }
    return this.historyData[this.historyData.length - 1];
  }

  goToPreviousRoute(filter?: {except: (route: string[]) => boolean})
    : {success: boolean, e?: Error} {
    const prev = this.getPreviousRouteData();
    if (!prev) {
      return {success: false, e: new Error('There is no previous route')};
    }
    if (this.isFleetingRoute(prev)) {
      return {success: false, e: new Error(`Previous route '${prev.paths.join('/')}' is fleeting.`)};
    }
    if (filter && filter.except(prev.paths)) {
      return {success: false, e: new Error('Given filter condition has failed to pass')};
    }
    const fullPath = '/' + prev.paths.join('/');
    const extras = { queryParams: prev.params, fragment: prev.fragment };
    this.router.navigate([fullPath], extras);
    return { success: true };
  }

  isFleetingRoute(routeData: RouteHistoryData): boolean {
    // null route is also 'fleeting' and should be redirected to default unresolved
    if (!routeData) { return true; }
    const pathJoined = routeData.paths.join('/');
    return (fleetingRoutesList.indexOf(pathJoined) === 0);
  }

  private parseURLData(url: string): RouteHistoryData {
    try {
      url = decodeURI(url);
      const original = url;
      let fragment = null;
      if (url.indexOf('#') >= 0) {
        const fragmentSplit = url.split('#'); fragmentSplit.shift();
        fragment = fragmentSplit.join('#');
        if (fragment) {
          const fragmentPosition = url.indexOf('#' + fragment);
          url = url.substr(0, fragmentPosition); // remove fragment part
        } else { fragment = null; }
      }
      const params = {};
      if (url.indexOf('?') >= 0) {
        const paramsPart = url.split('?')[1];
        const paramsArray = paramsPart.split('&');
        for (const param of paramsArray) {
          if (param.indexOf('=') >= 0) {
            const paramKeyValue = param.split('=');
            if (paramKeyValue[0]) { params[paramKeyValue[0]] = paramKeyValue[1]; }
          } else { params[param] = ''; }
        }
        if (paramsPart) {
          const paramsPosition = url.indexOf('?' + paramsPart);
          url = url.substr(0, paramsPosition);
        }
      }
      const paths = url.split('/'); paths.shift();
      const base = (paths.length === 0) ? '' : paths[0];
      return {
        url: original,
        base: base, // 1st level path
        paths: paths, // all dirs paths
        fragment: fragment,
        params: this.currentQueryParams
      };
    } catch (e) {
      console.log(e);
    }
  }

}
