/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClientModule, HttpClient } from '@angular/common/http';;
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { RouterTestingModule } from "@angular/router/testing";
import { HttpClientTestingModule } from "@angular/common/http/testing";

import { GridModule } from '../grid/grid.module';
import { ConsortiumService } from './shared/consortium.service';
import { ConsortiumComponent } from './consortium.component';
import { MockSharedModule } from "../shared/shared.module";
import { ConsortiumsListComponent } from "./consortiums-list/consortiums-list.component";
import { ConsortiumFormComponent } from "./consortium-form/consortium-form.component";

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('ConsortiumComponent', () => {
  let component: ConsortiumComponent;
  let fixture: ComponentFixture<ConsortiumComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
       MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientModule,
        HttpClientTestingModule,
        RouterTestingModule,
        GridModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [ConsortiumComponent, ConsortiumsListComponent, ConsortiumFormComponent ],
      providers: [
        ConsortiumService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConsortiumComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
