/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule, ModuleWithProviders } from '@angular/core';

import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ClarityModule } from '@clr/angular';

import { VmwContextualHelpModule } from '@vmw/ngx-contextual-help';
import { VmwComponentsModule, VmwTasksService } from '@vmw/ngx-components';
import { CspComponentsModule } from '@vmw/csp-ngx-components';
import { NgxChartsModule } from '@swimlane/ngx-charts';

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute, CanActivateChild, CanActivate, UrlSegment } from '@angular/router';

import { FeatureFlagService } from './feature-flag.service';
import { TranslateService } from '@ngx-translate/core';
import { FeatureFlagDirective } from './directives/feature-flag.directive';

import { defaultProvided } from './shared.module';

import { TourService as NgxTourService, TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { TourService } from './tour.service';

import { AgreementGuard } from './agreement-guard.service';
import { AuthenticatedGuard } from './authenticated-guard.service';

import { BlockchainService, MockBlockchainService } from '../blockchain/shared/blockchain.service';
import { MockBlockchainResolver, BlockchainResolver } from '../blockchain/shared/blockchain.resolver';
import { MockBlocksService, BlocksService } from '../blocks/shared/blocks.service';
import { MockTransactionService, TransactionsService } from '../transactions/shared/transactions.service';
import { SmartContractsService, MockSmartContractsService } from '../smart-contracts/shared/smart-contracts.service';
import { NodesService, MockNodesService } from '../nodes/shared/nodes.service';
import { OrgService, MockOrgService } from '../orgs/shared/org.service';
import { MockTranslateService, mockLanguagePack } from '../mocks/mock-translate.module';

import { HTTP_INTERCEPTORS } from '@angular/common/http';
import { MockRequestInterceptor } from '../app-interceptors';
import { MainModule } from '../main/main.module';
import { swaggerMocks, testController } from '../../test.controller';
export { swaggerMocks }; // re-export

import { BehaviorSubject } from 'rxjs';
import { TestBed } from '@angular/core/testing';

declare var require: any;

class MockGuard implements CanActivateChild, CanActivate {
  async canActivateChild() { return true; }
  async canActivate() { return true; }
}

interface ModuleInterface { imports?: any[]; providers?: any[]; provides?: any[]; exports?: any[]; declarations?: any[]; }

class MockInstances {
  blockchainsService: MockBlockchainService;
  blockchainResolver: MockBlockchainResolver;
  blocksService: MockBlocksService;
  transactionService: MockTransactionService;
  smartContractsService: MockSmartContractsService;
  nodesService: MockNodesService;
  orgService: MockOrgService;
  translateService: MockTranslateService;
  agreementGuard: MockGuard;
  authenticatedGuard: MockGuard;
  constructor() { this.reset(); }
  reset() {
    this.blockchainsService = new MockBlockchainService;
    this.blockchainResolver = new MockBlockchainResolver;
    this.blocksService = new MockBlocksService;
    this.transactionService = new MockTransactionService;
    this.smartContractsService = new MockSmartContractsService;
    this.nodesService = new MockNodesService;
    this.orgService = new MockOrgService;
    this.translateService = new MockTranslateService;
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
    { provide: HTTP_INTERCEPTORS, useClass: MockRequestInterceptor, multi: true, },

    { provide: BlockchainService, useValue: mocks.blockchainsService },
    { provide: BlockchainResolver, useValue: mocks.blockchainResolver },
    { provide: BlocksService, useValue: mocks.blocksService },
    { provide: TransactionsService, useValue: mocks.transactionService },
    { provide: SmartContractsService, useValue: mocks.smartContractsService },
    { provide: NodesService, useValue: mocks.nodesService },
    { provide: OrgService, useValue: mocks.orgService },

    { provide: TranslateService, useValue: mocks.translateService },

    { provide: AgreementGuard, useValue: mocks.agreementGuard },
    { provide: AuthenticatedGuard, useValue: mocks.authenticatedGuard },

    FeatureFlagService,
    FeatureFlagDirective,
    TourService,
    NgxTourService,
    VmwTasksService,
  ];
}

// Spec testing common imports
@NgModule({
  imports: [
    CommonModule,
    ReactiveFormsModule,
    RouterTestingModule,
    BrowserAnimationsModule,
    FormsModule,

    ClarityModule,
    NgxChartsModule,
    VmwContextualHelpModule.forRoot(),
    VmwComponentsModule.forRoot(),
    CspComponentsModule.forRoot(),
    TourNgxPopperModule.forRoot(),
  ],
  providers: defaultProvided,
})
export class SpecTestingModule {
  public imports: any[] = [];
  public provides: any[] = [];
  public exports: any[] = [];
  public declarations: any[] = [];
  public activatedRoute: MockActivatedRoute;
  public overrides: ModuleInterface;
  public testStatic: ModuleInterface;
  public fixed: ModuleInterface = { provides: [] };
  public fullModule: ModuleInterface;
  public services = {};

  public forTesting(): ModuleWithProviders {
    testController.forTesting = true;
    return {
      ngModule: SpecTestingModule,
      providers: testingSuiteBasicProvided()
    };
  }

  public init(obj: ModuleInterface = {}) {
    if (!obj) { obj = {}; }
    const def = this.merge(this.base(), obj);
    this.overrides = obj;
    this.testStatic = def;
    return def;
  }

  public merge(bottom: ModuleInterface, top: ModuleInterface) {
    const result: ModuleInterface = {
      imports: [].concat(arrval(bottom.imports), arrval(top.imports)),
      providers: [].concat(arrval(bottom.provides), arrval(bottom.providers), arrval(top.provides), arrval(top.providers)),
      exports: [].concat(arrval(bottom.exports), arrval(top.exports)),
      declarations: [].concat(arrval(bottom.declarations), arrval(top.declarations)),
    };
    return result;
  }

  public base() {
    const base: ModuleInterface = {
      imports: [].concat(this.imports),
      providers: [].concat(this.provides),
      exports: [].concat(this.exports),
      declarations: [].concat(this.declarations),
    };
    return base;
  }

  public importLanguagePack(lang: string = 'en') {
    if (!mockLanguagePack.langs[lang]) {
      const mockLanguagePackJSON = require(`../../static/i18n/${lang}.json`);
      const mockLanguagePackData = dotNotate(mockLanguagePackJSON);
      mockLanguagePack.langs[lang] = mockLanguagePackData;
      mockLanguagePack.setCurrentLang(lang);
    }
    mockLanguagePack.enabled = true;
  }

  public getActivatedRoute(options?: ActivatedRouteOptions) {
    if (!options) { options = {}; }
    const params = options.params ?  options.params : {};
    if (options.blockchainId) { params.blockchainId = options.blockchainId; }
    const fragment = options.fragment ? options.fragment : '';
    const data = options.data ? options.data : {};
    const queryParams = options.queryParams ? options.queryParams : {};
    const snapshot = options.snapshot ? options.snapshot : {
      params: params,
      fragment: fragment,
      data: data,
      queryParams: queryParams,
      outlet: options.outlet ? options.outlet : 'primary',
      url: options.url && options.url.length > 0 ? options.url : [
        new UrlSegment(options.blockchainId ? options.blockchainId : 'test', params)
      ]
    };
    const activatedRouteValue: MockActivatedRoute = {
      snapshot: snapshot,
      fragment: new BehaviorSubject<string>(fragment),
      data: new BehaviorSubject<ParamMap>(data),
      params: new BehaviorSubject<ParamMap>(params),
      queryParams: new BehaviorSubject<ParamMap>(queryParams),
      url: new BehaviorSubject<UrlSegment[]>(snapshot.url)
    };
    return activatedRouteValue;
  }

  public provideActivatedRoute(options?: ActivatedRouteOptions) {
    const route = this.getActivatedRoute(options);
    this.provides = this.provides.filter(prov => prov.provide !== ActivatedRoute); // remove old
    this.provides.push({ provide: ActivatedRoute, useValue: route, });
    return route;
  }

  public updateActivatedRoute(options?: ActivatedRouteOptions) {
    this.activatedRoute = this.getActivatedRoute(options);
    this.fullModule = this.merge(this.testStatic, this.fixed);
    TestBed.resetTestingModule();
    TestBed.configureTestingModule(this.fullModule).compileComponents();
  }

  public dynamicActivatedRoute() {
    this.fixed.provides.push({ provide: ActivatedRoute, useFactory: () => this.activatedRoute });
    this.activatedRoute = this.getActivatedRoute();
  }

  public getService(serviceType) {
    if (!this.services[serviceType]) {
      const serviceInstance = TestBed.get(serviceType);
      this.services[serviceType] = serviceInstance;
      this.provides.push({ provide: serviceType, useValue: serviceInstance });
      return serviceInstance;
    } else {
      return this.services[serviceType];
    }
  }

  public getTestingModules(): any[] {
    const fullList: any[] = [
      MainModule // Main already contains ALL submodules, including SharedModule
    ];
    return fullList;
  }

  public reset() {
    mockLanguagePack.enabled = false;
  }

}

interface ParamMap {
  [key: string]: any;
}
export interface MockActivatedRouteSnapshot {
  params?: ParamMap;
  queryParams?: ParamMap;
  data?: ParamMap;
  fragment?: string;
  outlet?: 'primary' | string;
  url?: UrlSegment[];
}

export interface ActivatedRouteOptions {
  snapshot?: MockActivatedRouteSnapshot;
  params?: ParamMap;
  queryParams?: ParamMap;
  data?: ParamMap;
  fragment?: string;
  outlet?: 'primary' | string;
  url?: UrlSegment[];
  blockchainId?: string;
}

export interface MockActivatedRoute {
  snapshot: MockActivatedRouteSnapshot;
  params: BehaviorSubject<ParamMap>;
  queryParams: BehaviorSubject<ParamMap>;
  data: BehaviorSubject<ParamMap>;
  url: BehaviorSubject<UrlSegment[]>;
  fragment: BehaviorSubject<string>;
}


export const getSpecTestingModule = (): SpecTestingModule => {
  const tester = new SpecTestingModule();
  tester.reset();
  tester.imports = [ tester.forTesting() ].concat( tester.getTestingModules() );
  return tester;
};


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

function arrval(a) {
  if (!a) { return []; }
  return a;
}

