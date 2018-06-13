import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule }    from '@angular/platform-browser';
import { ReactiveFormsModule } from '@angular/forms';  // <-- #1 import module
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { ClarityModule } from "@clr/angular";
import { HttpClientModule, HttpClient } from '@angular/common/http';
import { GridModule } from '../../grid/grid.module';

import { ActivatedRoute, Params } from '@angular/router';

import { BlockchainComponent } from './blockchain.component';
import { OrgListComponent } from '../../org-management/org-list/org-list.component';

import { TranslateService } from '@ngx-translate/core';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';


import { BlockchainsService } from '../shared/blockchains.service';
import { KubernetesService } from '../../kubernetes-management/shared/kubernetes.service';
import { OrgManagementService } from '../../org-management/shared/org-management.service';

export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './static/i18n/', '.json');
}

describe('BlockchainComponent', () => {
  let component: BlockchainComponent;
  let fixture: ComponentFixture<BlockchainComponent>;

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
      declarations: [
        BlockchainComponent,
        OrgListComponent,
      ],
      providers: [BlockchainsService, KubernetesService, OrgManagementService, {
        provide: ActivatedRoute,
        useValue: {
          params: {
            subscribe: (fn: (value) => void) => fn({
              value: 'add'
            }),
          },
        },
      }]

    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
