import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule, HttpClient } from '@angular/common/http';
import { ClarityModule } from "@clr/angular";
import { CommonModule } from '@angular/common';
import { GridModule } from '../grid/grid.module';

import { ActivatedRoute, Params } from '@angular/router';

import { TranslateService } from '@ngx-translate/core';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';

import { KubernetesManagementComponent } from './kubernetes-management.component';
import { KubernetesService } from './shared/kubernetes.service';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('KubernetesManagementComponent', () => {
  let component: KubernetesManagementComponent;
  let fixture: ComponentFixture<KubernetesManagementComponent>;

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
      declarations: [ KubernetesManagementComponent ],
      providers: [
        KubernetesService,
        TranslateService,
        {
          provide: ActivatedRoute,
          useValue: {
            params: {
              subscribe: (fn: (value: Params) => void) => fn({
                params: 'add',
              }),
            }
          },
        }
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(KubernetesManagementComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
