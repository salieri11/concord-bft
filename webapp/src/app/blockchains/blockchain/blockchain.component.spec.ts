/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientModule } from '@angular/common/http';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { BlockchainComponent } from './blockchain.component';
import { GridModule } from '../../grid/grid.module';
import { BlockchainsService } from '../shared/blockchains.service';
import { MockSharedModule } from '../../shared/shared.module';
import { OrgListComponent } from '../../orgs/org-list/org-list.component';
import { KubernetesService } from '../../kubernetes/shared/kubernetes.service';
import { OrgService } from '../../orgs/shared/org.service';

describe('BlockchainComponent', () => {
  let component: BlockchainComponent;
  let fixture: ComponentFixture<BlockchainComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientModule,
        FormsModule,
        GridModule
      ],
      declarations: [
        BlockchainComponent,
        OrgListComponent,
      ],
      providers: [
        BlockchainsService,
        KubernetesService,
        OrgService,
        TranslateService,
        {
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
