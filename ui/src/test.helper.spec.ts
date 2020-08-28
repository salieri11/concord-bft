/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule, ModuleWithProviders, NO_ERRORS_SCHEMA, Type } from '@angular/core';

import { CommonModule } from '@angular/common';

import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute, CanActivateChild, CanActivate, UrlSegment, Params, Router } from '@angular/router';

import { FeatureFlagService } from './app/shared/feature-flag.service';
import { TranslateService, TranslateModule } from '@ngx-translate/core';
import { FeatureFlagDirective } from './app/shared/directives/feature-flag.directive';

import { defaultProvided } from './app/shared/shared.module';

import { TourService as NgxTourService, TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { TourService } from './app/shared/tour.service';

import { AgreementGuard } from './app/shared/agreement-guard.service';
import { AuthenticatedGuard } from './app/shared/authenticated-guard.service';

import { AuthenticationService, MockAuthenticationService } from './app/shared/authentication.service';
import { BlockchainService, MockBlockchainService } from './app/blockchain/shared/blockchain.service';
import { MockBlockchainResolver, BlockchainResolver } from './app/blockchain/shared/blockchain.resolver';
import { MockBlocksService, BlocksService } from './app/blocks/shared/blocks.service';
import { MockTransactionService, TransactionsService } from './app/transactions/shared/transactions.service';
import { SmartContractsService, MockSmartContractsService } from './app/smart-contracts/shared/smart-contracts.service';
import { NodesService, MockNodesService } from './app/nodes/shared/nodes.service';
import { OrgService, MockOrgService } from './app/orgs/shared/org.service';
import { MockTranslateService, mockLanguagePack } from './app/mocks/mock-translate.module';

import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';
import { MockRequestInterceptor } from './app/app-interceptors';

import { testController } from './test.controller';

import { BehaviorSubject } from 'rxjs';
import { TestBed, ComponentFixture, TestBedStatic, inject } from '@angular/core/testing';
import { MockRouteService, RouteService } from './app/shared/route.service';
import { ReactiveFormsModule } from '@angular/forms';

declare var require: any;

const LANGUAGE_PACK_FOLDER = './static/i18n';

Error.stackTraceLimit = 10;
jasmine.getEnv().allowRespy(true); // ! important; allows respying on functions that are otherwise recreated manually.

export interface ModuleInterface {
  imports?: any[];
  providers?: any[]; provides?: any[];
  exports?: any[];
  declarations?: any[];
  schemas?: any[];
}

export interface TestEnvironment extends ModuleInterface {
  route?: string | ActivatedRouteOptions;
  router?: string;
}

export class MockGuard implements CanActivateChild, CanActivate {
  async canActivateChild() { return true; }
  async canActivate() { return true; }
}

export class MockInstances {
  authenticationService: MockAuthenticationService;
  blockchainsService: MockBlockchainService;
  blockchainResolver: MockBlockchainResolver;
  blocksService: MockBlocksService;
  transactionService: MockTransactionService;
  smartContractsService: MockSmartContractsService;
  nodesService: MockNodesService;
  orgService: MockOrgService;
  translateService: MockTranslateService;
  routeService: MockRouteService;
  agreementGuard: MockGuard;
  authenticatedGuard: MockGuard;
  constructor() { this.reset(); }
  reset() {
    this.authenticationService = new MockAuthenticationService;
    this.blockchainsService = new MockBlockchainService;
    this.blockchainResolver = new MockBlockchainResolver;
    this.blocksService = new MockBlocksService;
    this.transactionService = new MockTransactionService;
    this.smartContractsService = new MockSmartContractsService;
    this.nodesService = new MockNodesService;
    this.orgService = new MockOrgService;
    this.translateService = new MockTranslateService;
    this.routeService = new MockRouteService;
    this.agreementGuard = new MockGuard;
    this.authenticatedGuard = new MockGuard;
  }
}

// All mock instances
export const mocks = new MockInstances;

// All default provides for spec testing
function testingSuiteBasicProvided() {
  mocks.reset();
  return [
    { provide: Function, useValue: new Function },
    { provide: HTTP_INTERCEPTORS, useClass: MockRequestInterceptor, multi: true, },

    { provide: AuthenticationService, useValue: mocks.authenticationService },
    { provide: BlockchainService, useValue: mocks.blockchainsService },
    { provide: BlockchainResolver, useValue: mocks.blockchainResolver },
    { provide: BlocksService, useValue: mocks.blocksService },
    { provide: TransactionsService, useValue: mocks.transactionService },
    { provide: SmartContractsService, useValue: mocks.smartContractsService },
    { provide: NodesService, useValue: mocks.nodesService },
    { provide: OrgService, useValue: mocks.orgService },

    { provide: TranslateService, useValue: mocks.translateService },
    { provide: RouteService, useValue: mocks.routeService },

    { provide: AgreementGuard, useValue: mocks.agreementGuard },
    { provide: AuthenticatedGuard, useValue: mocks.authenticatedGuard },

    FeatureFlagService,
    FeatureFlagDirective,
    TourService,
    NgxTourService,
  ];
}

// Spec testing common imports
@NgModule({
  imports: [
    CommonModule,
    RouterTestingModule,
    TourNgxPopperModule.forRoot(),
  ],
  providers: defaultProvided,
})
export class SpecTestingHelper<T = any> {
  component: T;
  fixture: ComponentFixture<T>;
  componentType: Type<T>;

  imports: any[] = [ // default imports
    TranslateModule,
    HttpClientModule,
    ReactiveFormsModule,
  ];
  provides: any[] = [];
  exports: any[] = [];
  declarations: any[] = [];
  schemas: any[] = [ NO_ERRORS_SCHEMA ];

  router: MockRouter;
  routerDefaultPath: string = '/';
  activatedRoute: MockActivatedRoute<T>;
  defaultActivatedRouteOptions: ActivatedRouteOptions;

  overrides: ModuleInterface;
  testStatic: ModuleInterface;
  fixed: ModuleInterface = { provides: [] };
  fullModule: ModuleInterface;
  services = {};

  private testModuleResetFunction: () => TestBedStatic;

  constructor(componentType: Type<T>) {
    this.componentType = componentType;
  }

  forTesting(): ModuleWithProviders {
    testController.forTesting = true;
    return {
      ngModule: SpecTestingHelper,
      providers: testingSuiteBasicProvided() as any
    };
  }

  init(obj: ModuleInterface = {}) {
    if (!obj) { obj = {}; }
    const def = this.merge(this.base(), obj);
    this.overrides = obj;
    this.testStatic = def;
    this.activatedRoute = this.getActivatedRoute(); // provide default rout
    def.providers.forEach((value, i) => { // if just given type; modified to conform provide syntax
      if (this.isService(value)) { def.providers[i] = { provide: value, useClass: value }; }
    });
    if (this.isService(this.componentType)) { // Override to provide real service when that service is the one being tested.
      def.providers.push({ provide: this.componentType, useClass: this.componentType});
    }
    return def;
  }

  importLanguagePack(lang: string = 'en') {
    if (!mockLanguagePack.langs[lang]) {
      const mockLanguagePackJSON = require(`${LANGUAGE_PACK_FOLDER}/${lang}.json`);
      const mockLanguagePackData = dotNotate(mockLanguagePackJSON);
      mockLanguagePack.langs[lang] = mockLanguagePackData;
      mockLanguagePack.setCurrentLang(lang);
    }
    mockLanguagePack.enabled = true;
  }

  updateActivatedRoute(options?: ActivatedRouteOptions) {
    const newRoute = this.getActivatedRoute(options);
    for (const routeProp of Object.keys(this.activatedRoute)) {
      this.activatedRoute[routeProp] = newRoute[routeProp];
    }
  }

  getService<T1 = any, T2 = T1>(type: Type<T1>, as?: Type<T2>): T2 {
    devnull(as);
    const serviceName = type.prototype.constructor.name;
    if (!this.services[serviceName]) {
      const serviceInstance = TestBed.get(type);
      this.services[serviceName] = { instance: serviceInstance, type: type };
      this.provides.push({ provide: type, useFactory: () => this.services[serviceName].instance });
      return serviceInstance;
    } else {
      return this.services[serviceName].instance;
    }
  }

  reloadServices() {
    for (const serviceName of Object.keys(this.services)) {
      const type = this.services[serviceName].type;
      const serviceInstance = TestBed.get(type);
      this.services[serviceName].instance = serviceInstance;
    }
  }

  reset() {
    mockLanguagePack.enabled = false;
    this.activatedRoute = {};
    this.defaultActivatedRouteOptions = null;
  }

  expedite(env: TestEnvironment, _beforeTesting?: () => void, _prepareEach?: () => void) {
    this.testModuleResetFunction = TestBed.resetTestingModule;
    beforeAll(done => (async () => {
      TestBed.resetTestingModule();
      testController.currentSpec = this.componentType.prototype.constructor.name;
      if (env.router) {
        if (!env.providers) { env.providers = []; }
        this.routerDefaultPath = env.router;
        this.newMockRouter();
        env.providers.push({ provide: Router, useFactory: () => this.router });
      }
      if (env.route) { // default activated route if given
        const routeOptions: ActivatedRouteOptions = typeof env.route === 'string' ?
                                { url: [new UrlSegment(env.route as string, {})] }
                               : env.route as ActivatedRouteOptions;
        this.defaultActivatedRouteOptions = routeOptions;
      } else {
        this.defaultActivatedRouteOptions = {};
      }
      // Compile provided modules, components, etc.
      TestBed.configureTestingModule(this.init(env)).compileComponents();
      await TestBed.compileComponents();
      const isService = this.isService(this.componentType);
      if (isService) {
        this.fixture = null;
        this.component = TestBed.get(this.componentType);
        it('provides service instance ', inject([this.componentType], service => {
          this.component = service;
        }));
      }
      if (_beforeTesting) {
        try {
          _beforeTesting();
        } catch (e) {
          console.error('beforeTesting in ' + testController.currentSpec, e);
        }
      }
      if (!isService) { this.resetComponent(); }

      // Prevent Angular from resetting testing module;
      // Resetting for every test case is too damn slow!
      TestBed.resetTestingModule = () => TestBed;
    })().then(done).catch(done.fail));

    beforeEach(() => {
      this.updateActivatedRoute(this.defaultActivatedRouteOptions);
      if (this.router) { this.router.reset(this.routerDefaultPath); }
      if (_prepareEach) {
        try {
          _prepareEach();
        } catch (e) {
          console.error('prepareEach in ' + testController.currentSpec, e);
        }
      }
    });

    afterEach(() => {
      if (this.fixture) { this.fixture.destroy(); }
    });

    afterAll(() => {
      // reinstate resetTestingModule method
      if (this.testModuleResetFunction) {
        TestBed.resetTestingModule = this.testModuleResetFunction;
      }
      TestBed.resetTestingModule();
    });
    return this;
  }

  createComponent(skipChangeDetection?: boolean) {
    if (this.isService(this.componentType)) {
      this.fixture = null;
      this.component = TestBed.get(this.componentType);
    } else {
      this.fixture = TestBed.createComponent(this.componentType);
      this.component = this.fixture.componentInstance;
      if (!skipChangeDetection) { this.fixture.detectChanges(); }
    }
  }

  resetComponent(skipChangeDetection?: boolean) { this.createComponent(skipChangeDetection); }

  refreshComponent(skipChangeDetection?: boolean) { this.createComponent(skipChangeDetection); }

  componentProperty(propname: string) { return this.component[propname]; }

  isService(a?) {
    if (!a) { a = this.componentType; }
    return (a && typeof a === 'function'
            && Object.keys(a).indexOf('ngInjectableDef') >= 0)
            && a['ngInjectableDef']['providedIn'] === 'root';
  }

  newMockRouter() {
    if (this.router) {
      this.router.reset(this.routerDefaultPath);
    } else {
      this.router = newMockRouter(this.routerDefaultPath);
    }
  }

  private getActivatedRoute(options?: ActivatedRouteOptions) {
    if (!options) { options = {}; }
    const params = options.params ?  options.params : {};
    if (options.blockchainId) { params.blockchainId = options.blockchainId; }
    const fragment = options.fragment ? options.fragment : '';
    const data = options.data ? options.data : {};
    const queryParams = options.queryParams ? options.queryParams : {};
    const urlSegments: UrlSegment[] = [];
    if (!options.url) {
      urlSegments.push(new UrlSegment(options.blockchainId ? options.blockchainId : 'test', params));
    } else {
      for (const urlData of options.url) { urlSegments.push(new UrlSegment(urlData.path, urlData.params)); }
    }
    const snapshot = options.snapshot ? options.snapshot : {
      params: params,
      fragment: fragment,
      data: data,
      queryParams: queryParams,
      outlet: options.outlet ? options.outlet : 'primary',
      url: urlSegments
    };
    const activatedRouteValue: MockActivatedRoute<T> = {
      component: this.componentType,
      snapshot: snapshot,
      fragment: new BehaviorSubject<string>(fragment),
      data: new BehaviorSubject<Params>(data),
      params: new BehaviorSubject<Params>(params),
      queryParams: new BehaviorSubject<Params>(queryParams),
      url: new BehaviorSubject<UrlSegment[]>(urlSegments)
    };
    return activatedRouteValue;
  }

  private merge(bottom: ModuleInterface, top: ModuleInterface) {
    const result: ModuleInterface = {
      imports: [].concat(arrval(bottom.imports), arrval(top.imports)),
      providers: [].concat(arrval(bottom.provides), arrval(bottom.providers), arrval(top.provides), arrval(top.providers)),
      exports: [].concat(arrval(bottom.exports), arrval(top.exports)),
      declarations: [].concat(arrval(bottom.declarations), arrval(top.declarations)),
      schemas: [].concat(arrval(bottom.schemas), arrval(top.schemas)),
    };
    return result;
  }

  private base() {
    const base: ModuleInterface = {
      imports: [].concat(this.imports),
      providers: [{ provide: ActivatedRoute, useFactory: () => this.activatedRoute }].concat(this.provides),
      exports: [].concat(this.exports),
      declarations: [].concat(this.declarations),
      schemas: [].concat(this.schemas),
    };
    return base;
  }

}


export interface MockActivatedRouteSnapshot {
  params?: Params;
  queryParams?: Params;
  data?: Params;
  fragment?: string;
  outlet?: 'primary' | string;
  url?: UrlSegment[];
}

export interface ActivatedRouteOptions {
  snapshot?: MockActivatedRouteSnapshot;
  params?: Params;
  queryParams?: Params;
  data?: Params;
  fragment?: string;
  outlet?: 'primary' | string;
  url?: { path: string; params?: any; }[];
  blockchainId?: string;
}

export interface MockActivatedRoute<T> {
  component?: Type<T>;
  snapshot?: MockActivatedRouteSnapshot;
  params?: BehaviorSubject<Params>;
  queryParams?: BehaviorSubject<Params>;
  data?: BehaviorSubject<Params>;
  url?: BehaviorSubject<UrlSegment[]>;
  fragment?: BehaviorSubject<string>;
}

export class MockRouter {
  url: string = '/';
  constructor(url: string) { this.reset(url); }
  navigate() { return new Promise(resolve => { resolve(true); }); }
  reset(url: string) {
    this.navigate = () => new Promise(resolve => { resolve(true); });
    this.url = url;
  }
}

export function testFor<T = any>(componentType: Type<T>): SpecTestingHelper<T> {
  const tester = new SpecTestingHelper<T>(componentType);
  tester.reset();
  tester.imports = [ tester.forTesting() ].concat(tester.imports);
  return tester;
}

export function beforeTesting(fn: () => void) { return fn; }

export function prepareEach(fn: () => void) {  return fn; }

function newMockRouter(url?: string) { return new MockRouter(url); }

function dotNotate(obj, target?, prefix?) {
  target = target || {},
  prefix = prefix || '';
  Object.keys(obj).forEach(function(key) {
    if ( typeof(obj[key]) === 'object' ) {
      dotNotate(obj[key], target, prefix + key + '.');
    } else {
      return target[prefix + key] = obj[key];
    }
  });
  return target;
}

function arrval(a) { if (!a) { return []; } return a; }

function devnull(a) { return a; }
