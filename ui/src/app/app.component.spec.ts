/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, async } from '@angular/core/testing';

import { TourNgxPopperModule } from 'ngx-tour-ngx-popper';
import { RouterTestingModule } from '@angular/router/testing';
import { MockSharedModule } from './shared/shared.module';

import { AppComponent } from './app.component';

describe('AppComponent', () => {
  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        TourNgxPopperModule.forRoot(),
        RouterTestingModule,
        MockSharedModule
      ],
      declarations: [
        AppComponent
      ]
    }).compileComponents();
  }));

  it('should create the app', async(() => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.debugElement.componentInstance;
    expect(app).toBeTruthy();
  }));
});
