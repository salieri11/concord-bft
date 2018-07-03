/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { HttpClient } from '@angular/common/http';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { HttpClientTestingModule } from "@angular/common/http/testing";
import { RouterTestingModule } from "@angular/router/testing";

import { GridModule } from '../grid/grid.module';
import { KubernetesService } from './shared/kubernetes.service';
import { KubernetesComponent } from './kubernetes.component';
import { MockSharedModule } from "../shared/shared.module";
import { KubernetesListComponent } from "./kubernetes-list/kubernetes-list.component";
import { KubernetesFormComponent } from "./kubernetes-form/kubernetes-form.component";

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('KubernetesComponent', () => {
  let component: KubernetesComponent;
  let fixture: ComponentFixture<KubernetesComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
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
      declarations: [ KubernetesComponent, KubernetesListComponent, KubernetesFormComponent ],
      providers: [
        KubernetesService
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(KubernetesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
