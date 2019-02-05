/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { Observable } from 'rxjs';
import { ClarityModule } from '@clr/angular';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { HttpClientModule, HttpClient } from '@angular/common/http';

import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';

import { GridComponent } from './grid.component';

import { ErrorAlertService } from '../shared/global-error-handler.service';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('GridComponent', () => {
  let component: GridComponent;
  let fixture: ComponentFixture<GridComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule,
        RouterTestingModule,
        CommonModule,
        HttpClientModule,
        FormsModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [GridComponent],
      providers: [ErrorAlertService]

    })
      .compileComponents();

  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(GridComponent);
    component = fixture.componentInstance;
    component.options = {
      columns: [],
      getData: () => {
        return new Observable(observer => {
          observer.next({
            meta: {
              total: 10,
              size: 50,
              totalPages: 5
            },
            objects: []
          });
          observer.complete();
        });
      },
      paginationTitle: 'string'
    };
      fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
