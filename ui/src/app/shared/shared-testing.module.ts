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
import { ActivatedRoute, CanActivateChild, CanActivate } from '@angular/router';

import { BlockchainService, MockBlockchainsService,
  MockBlockchainResolver, BlockchainResolver
} from '../blockchain/shared/blockchain.service';
import { FeatureFlagService } from './feature-flag.service';
import { TranslateService } from '@ngx-translate/core';
import { FeatureFlagDirective } from './directives/feature-flag.directive';
import { MockTranslateService, mockLanguagePack } from '../mocks/mock-translate.module';

import { defaultProvided } from './shared.module';

import { TourService as NgxTourService, TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { TourService } from './tour.service';

import { AgreementGuard } from './agreement-guard.service';
import { AuthenticatedGuard } from './authenticated-guard.service';
import { SwaggerComponent, MockSwaggerComponent } from '../developer/swagger/swagger.component';
import { MockBlocksService, BlocksService } from '../blocks/shared/blocks.service';
import { SmartContractsService, MockSmartContractsService } from '../smart-contracts/shared/smart-contracts.service';
import { HTTP_INTERCEPTORS } from '@angular/common/http';
import { MockRequestInterceptor } from '../app-interceptors';
import { MainModule } from '../main/main.module';

declare var require: any;

class MockGuard implements CanActivateChild, CanActivate {
  async canActivateChild() { return true; }
  async canActivate() { return true; }
}

interface ModuleInterface { imports?: any[]; provides?: any[]; exports?: any[]; declarations?: any[]; }

// All default provides for spec testing
const testingSuiteBasicProvided = [
  { provide: HTTP_INTERCEPTORS, useClass: MockRequestInterceptor, multi: true, },

  { provide: BlockchainService, useClass: MockBlockchainsService },
  { provide: BlocksService, useClass: MockBlocksService },
  { provide: BlockchainResolver, useClass: MockBlockchainResolver },
  { provide: SmartContractsService, useClass: MockSmartContractsService },

  { provide: TranslateService, useClass: MockTranslateService },

  { provide: AgreementGuard, useClass: MockGuard },
  { provide: AuthenticatedGuard, useClass: MockGuard },

  { provide: SwaggerComponent, useClass: MockSwaggerComponent },
  FeatureFlagService,
  FeatureFlagDirective,
  TourService,
  NgxTourService,
  VmwTasksService,
];

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

  public forTesting(): ModuleWithProviders {
    return {
      ngModule: SpecTestingModule,
      providers: testingSuiteBasicProvided
    };
  }

  public init(obj: ModuleInterface = {}) {
    if (!obj) { obj = {}; }
    const def: ModuleInterface = {
      imports: this.imports,
      provides: this.provides,
      exports: this.exports,
      declarations: this.declarations,
    };
    if (obj && Array.isArray(obj.imports)) { def.imports = def.imports.concat(obj.imports); }
    if (obj && Array.isArray(obj.provides)) { def.provides = def.provides.concat(obj.provides); }
    if (obj && Array.isArray(obj.exports)) { def.exports = def.exports.concat(obj.exports); }
    if (obj && Array.isArray(obj.declarations)) { def.declarations = def.declarations.concat(obj.declarations); }
    return def;
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

  // TODO: make providing ActivateRoute more robust with easy options
  // This would be quite frequently used.
  public provideActivatedRoute(options?) {
    const params = (options && options.params) ?  options.params : { consortiumId: 1 };
    const fragment = (options && options.fragment) ? options.fragment : '';
    const data = (options && options.data) ? options.data : {};
    const queryParams = (options && options.queryParams) ? options.queryParams : {};
    const snapshot = (options && options.snapshot) ? options.snapshot : { params: { consortiumId: 'test' } };
    this.provides.push({
      provide: ActivatedRoute,
      useValue: {
        snapshot: snapshot,
        fragment: { subscribe: (fn: (value) => void) => fn(fragment), },
        data: { subscribe: (fn: (value) => void) => fn(data), },
        params: { subscribe: (fn: (value) => void) => fn(params), },
        queryParams: { subscribe: (fn: (value) => void) => fn(queryParams), },
      },
    });
  }

  public getTestingModules(): any[] {
    const fullList: any[] = [
      MainModule // Main already contains ALL submodules, including SharedModule
    ];
    return fullList;
  }

  public reset() {
    this.provides = [];
    mockLanguagePack.enabled = false;
  }

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

