/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { BlockchainService } from '../blockchain/shared/blockchain.service';
import { BlockchainResolver } from '../blockchain/shared/blockchain.resolver';
import { Router, NavigationEnd, ActivatedRoute, NavigationStart, DefaultUrlSerializer } from '@angular/router';
import { mainRoutes, fleetingRoutesList, ConsortiumStates, uuidRegExp } from './urls.model';
import { DeployStates } from '../blockchain/shared/blockchain.model';
import { Subject } from 'rxjs';

const urlSerializer = new DefaultUrlSerializer();

interface RouteHistoryData {
  action?: string;
  request?: RouteHistoryData;
  url: string;
  base: string;
  paths: string[];
  fragment: string;
  currentQueryParams: { [name: string]: string; };
  params?: { [name: string]: string; };
}

@Injectable({
  providedIn: 'root'
})
export class RouteService {

  public static linkActionIdentifier = '/ng-link-action/';

  historyData: RouteHistoryData[] = [];
  currentRouteData: RouteHistoryData;
  outletEnabled: boolean = true;
  readonly linkAction: Subject<RouteHistoryData>  = new Subject<RouteHistoryData>();

  private initialized: boolean = false;
  private currentQueryParams: any;
  private outputAllRouterEvents = false;
  private unfinishedTaskIds: string[] = [];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private blockchainService: BlockchainService,
    private blockchainResolver: BlockchainResolver,
  ) { this.initialize(); }

  public static isLinkAction(url: string) {
    return url.startsWith(RouteService.linkActionIdentifier);
  }

  public static createLinkAction(data: {text: string, action: string, params: object}) {
    return `[${data.text}](${RouteService.getLinkActionPath(data.action, data.params)})`;
  }

  public static getLinkActionPath(actionName: string, params: object) {
    const paramsArray = [];
    for (const paramName of Object.keys(params)) {
      paramsArray.push(encodeURIName(paramName) + '=' + encodeURIName(params[paramName]));
    }
    return RouteService.linkActionIdentifier + actionName + '?' + paramsArray.join('&');
  }

  async resolveConsortium() {
    if (this.blockchainService.blockchains) { return; } // already resolved;
    await this.blockchainResolver.resolve().toPromise();
  }

  initialize() {
    if (this.initialized) { return; }
    if (this.route.queryParams) {
      this.route.queryParams.subscribe(params => {
        this.currentQueryParams = JSON.parse(JSON.stringify(params));
      });
    }
    this.router.events.subscribe(e => {
      if (this.outputAllRouterEvents) { console.log(e); }
      if (e instanceof NavigationStart && RouteService.isLinkAction(e.url)) {
        return this.triggerLinkActionEvent(e.url);
      }
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
    let path;
    fragment = fragment ? fragment : null;
    if (this.outputAllRouterEvents) {
      console.log(new Error('Redirect to default called from stack trace:'));
    }
    if (this.blockchainService.blockchains && this.blockchainService.blockchains.length > 0) {
      let blockchainId = this.blockchainService.loadSelectedBlockchain();
      if (!blockchainId || this.blockchainService.blockchains.filter(
                            item => item.id === blockchainId).length === 0
        ) {
          blockchainId = this.blockchainService.blockchains[0].id;
      }
      if (blockchainId) {
        // Force route recheck; outletEnabled true|false does not work anymore
        path = '/' + blockchainId + '/' + mainRoutes.dashboard;
        const reuseConfigSaved = this.router.routeReuseStrategy.shouldReuseRoute;
        this.router.routeReuseStrategy.shouldReuseRoute = () => false;
        this.router.navigate([path], { fragment: fragment })
          .then(() => { this.router.routeReuseStrategy.shouldReuseRoute = reuseConfigSaved; });
        return;
      }
    }
    // No consortium joined, or blockchainId found; redirect to welcome to let users deploy
    path = '/' + mainRoutes.blockchain + '/' + mainRoutes.welcome;
    this.router.navigate([path]);
  }

  goToDeploying(taskId: string): boolean {
    if (taskId === ConsortiumStates.waiting || uuidRegExp.test(taskId)) {
      this.router.navigate(mainRoutes.deployingBaseRoute.concat([taskId]));
      return true;
    }
    return false;
  }

  async resumeUnfinishedDeployIfExists(): Promise<boolean> {
    const deployingRegistry = this.blockchainService.loadDeployingRegistry();
    this.unfinishedTaskIds = [];
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
          this.unfinishedTaskIds.push(deployDataKey);
        } catch (e) { console.log(e); }
      }
    }
    this.blockchainService.saveDeployingRegistry(deployingRegistry);

    if (this.unfinishedTaskIds.length > 0) {
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

  triggerLinkActionEvent(url: string) {
    if (!RouteService.isLinkAction(url)) { return; }
    const routeData = this.parseURLData(url);
    routeData.action = routeData.paths[1];
    this.linkAction.next(routeData);
    if (routeData.params && routeData.params.redirectTo) {
      this.router.navigate([routeData.params.redirectTo]);
    }
  }

  //
  // Handle /login-return redirects
  // Check to see if the login-return was to reauthenticate,
  // if so redirect to last location, otherwise redirect to default
  //
  loginReturnHandler(): boolean {
    const twoMinutes = 2 * 60 * 1000;
    const lastLocation = localStorage.getItem('lastLocation');

    if (lastLocation) {
      const locItems = lastLocation.split('--');
      const lastLocDate = +new Date(locItems[0]);
      const valid = ((+new Date) - lastLocDate) < twoMinutes;

      // Expire redirect after two minutes
      if (valid) {
        this.router.navigateByUrl(locItems[1]);
        return false;
      }
    }

    this.redirectToDefault();
    return false;
  }

  parseURLData(url: string): RouteHistoryData {
    return parsePathUrl(url, this.currentQueryParams);
  }

}

function encodeURIName(text: string) {
  text = encodeURIComponent(text);
  return text.replace(/\(/g, '%28').replace(/\)/g, '%29');
}

function parsePathUrl(url: string, currentQueryParams?) {
  const original = url;
  try {
    const parsed = urlSerializer.parse(url);
    const segs = parsed.root.children.primary.segments;
    const paths = [];
    for (const seg of segs) { paths.push(seg.path); }
    return {
      url: original,
      base: paths[0], // 1st level path
      paths: paths, // all dirs paths
      fragment: parsed.fragment, // parsed fragment from given url
      params: parsed.queryParamMap as {}, // parsed params from given url
      currentQueryParams: currentQueryParams, // Query params from ActivatedRoute
    };
  } catch (e) {
    console.error(e);
    return { url: original, base: '', paths: [], fragment: null, params: {}, currentQueryParams: null };
  }
}



export class MockRouteService {

  public static linkActionIdentifier = '/ng-link-action/';

  historyData: RouteHistoryData[] = [];
  currentRouteData: RouteHistoryData;
  outletEnabled: boolean = true;
  readonly linkAction: Subject<RouteHistoryData>  = new Subject<RouteHistoryData>();

  public static isLinkAction() {}
  public static createLinkAction() {}
  public static getLinkActionPath() {}

  async resolveConsortium() {}
  async resumeUnfinishedDeployIfExists() {}

  initialize() {}
  isPathAlreadyActive() {}
  redirectToDefault() {}
  goToDeploying() {}
  getPreviousRouteData() {}
  goToPreviousRoute() {}
  isFleetingRoute() {}
  triggerLinkActionEvent() {}
  loginReturnHandler() {}
  parseURLData(url: string) { return parsePathUrl(url); }

  resetAll() {

  }

}
