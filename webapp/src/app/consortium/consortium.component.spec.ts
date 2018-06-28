/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ActivatedRoute } from '@angular/router';

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule, HttpClient } from '@angular/common/http';
import { ClarityModule } from '@clr/angular';
import { CommonModule } from '@angular/common';
import { GridModule } from '../grid/grid.module';

import { TranslateService } from '@ngx-translate/core';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';

import { ConsortiumService } from './shared/consortium.service';
import { ConsortiumComponent } from './consortium.component';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('ConsortiumComponent', () => {
  let component: ConsortiumComponent;
  let fixture: ComponentFixture<ConsortiumComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        CommonModule,
        ClarityModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientModule,
        ReactiveFormsModule,
        FormsModule,
        GridModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [ConsortiumComponent],
      providers: [
        ConsortiumService,
        TranslateService,
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'add'
              ),
            },
          },
        }
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
