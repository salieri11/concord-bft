/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule, Injector, APP_INITIALIZER } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { RouterModule } from '@angular/router';
import { LOCATION_INITIALIZED } from '@angular/common';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { TranslateLoader, TranslateModule, TranslateService } from '@ngx-translate/core';
import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';

import { ClarityModule } from '@clr/angular';

import { AppComponent } from './app.component';
import { MainModule } from './main/main.module';
import { ErrorsModule } from './errors/errors.module';
import { AppRoutingModule } from './app-routing.module';
import { MarketingModule } from './marketing/marketing.module';
import { VmwClarityThemeService } from './shared/theme.provider';
import { VIPModule } from '@vmw/ngx-vip';

import { AppInitService } from './app.init';
import { MainComponent } from './main/main/main.component';
import { AuthenticatedGuard } from './shared/authenticated-guard.service';
import { AgreementGuard } from './shared/agreement-guard.service';
import { BlockchainResolver } from './blockchain/shared/blockchain.service';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, 'static/i18n/', '.json');
}

export function langInitializerFactory(translate: TranslateService, injector: Injector, themeService: VmwClarityThemeService) {
  return () => new Promise<any>((resolve: any) => {
    themeService.initialize();
    const locationInitialized = injector.get(LOCATION_INITIALIZED, Promise.resolve(null));
    locationInitialized.then(() => {
      const defaultLang = 'en';
      const browserLang = translate.getBrowserLang() || defaultLang;
      const browserCultureLang = translate.getBrowserCultureLang() || browserLang || defaultLang;
      const languages = [browserCultureLang, browserLang, defaultLang];
      translate.setDefaultLang(defaultLang);

      // Check each language in order of specificity to handle missing i18n files and provide a fallback
      initLanguage(translate, languages, resolve);
    });
  });
}

function initLanguage(translate: TranslateService, languages: string[], resolve: any) {
  const languageMap = {
    'zh': 'zh-CN'
  };

  if (languages.length === 0) {
    return;
  }
  const mappedLanguage = languageMap[languages[0]] || languages[0];

  translate.use(mappedLanguage).subscribe(() => {
    console.info(`Successfully initialized '${mappedLanguage}' language.'`);
  }, err => {
    console.error(`Problem with '${mappedLanguage}' language initialization: ${err}`);
    languages.splice(0, 1);
    initLanguage(translate, languages, resolve);
  }, () => {
    resolve(null);
  });
}

export function init_app(appLoadService: AppInitService) {
  return () => appLoadService.init();
}

@NgModule({
  declarations: [
    AppComponent,
  ],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    AppRoutingModule,
    VIPModule,
    ErrorsModule,
    MainModule,
    ClarityModule,
    RouterModule,
    TourNgxPopperModule.forRoot(),
    MarketingModule,
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient]
      }
    }),
    RouterModule.forChild([{
      // Redirect all unmatched to main dahboard
      path: '**', // must be added AFTER all sub-modules (such as MainModule, SharedModule)
      canActivate: [AuthenticatedGuard, AgreementGuard],
      resolve: {blockchain: BlockchainResolver},
      component: MainComponent
    }]),
  ],
  providers: [
    AppInitService,
    {
      provide: APP_INITIALIZER,
      useFactory: init_app,
      deps: [AppInitService],
      multi: true
    },
    VmwClarityThemeService,
    {
      provide: APP_INITIALIZER,
      useFactory: langInitializerFactory,
      deps: [TranslateService, Injector, VmwClarityThemeService],
      multi: true
    },
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
}
