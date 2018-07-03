/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule, Injector, APP_INITIALIZER } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { LOCATION_INITIALIZED } from '@angular/common';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { TranslateLoader, TranslateModule, TranslateService } from '@ngx-translate/core';

import { ClarityModule } from '@clr/angular';

import { AppComponent } from './app.component';
import { MainModule } from './main/main.module';
import { AppRoutingModule } from './app-routing.module';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, '/assets/static/i18n/', '.json');
}

export function langInitializerFactory(translate: TranslateService, injector: Injector) {
  return () => new Promise<any>((resolve: any) => {
    const locationInitialized = injector.get(LOCATION_INITIALIZED, Promise.resolve(null));
    locationInitialized.then(() => {
      const langToSet = 'en';
      translate.setDefaultLang('en');
      translate.use(langToSet).subscribe(() => {
        console.info(`Successfully initialized '${langToSet}' language.'`);
      }, err => {
        console.error(`Problem with '${langToSet}' language initialization: ${err}`);
      }, () => {
        resolve(null);
      });
    });
  });
}

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    MainModule,
    ClarityModule,
    AppRoutingModule,
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient]
      }
    })
  ],
  providers: [
    {
      provide: APP_INITIALIZER,
      useFactory: langInitializerFactory,
      deps: [TranslateService, Injector],
      multi: true
    },
  ],
  bootstrap: [AppComponent]
})
export class AppModule {
}
