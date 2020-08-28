/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';  // <-- #1 import module
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { GridModule } from '../grid/grid.module';
import { OrgListComponent } from './org-list/org-list.component';
import { OrgService } from './shared/org.service';
import { MockSharedModule } from '../shared/shared.module';
import { OrgsComponent } from './orgs.component';
import { InivteUserComponent } from './inivte-user/inivte-user.component';

import { BlockchainService, MockBlockchainService } from './../blockchain/shared/blockchain.service';

describe('OrgsComponent', () => {
  let component: OrgsComponent;
  let fixture: ComponentFixture<OrgsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        BrowserAnimationsModule,
        BrowserModule,
        HttpClientTestingModule,
        FormsModule,
        GridModule
      ],
      declarations: [
        OrgsComponent,
        OrgListComponent,
        InivteUserComponent
      ],
      providers: [
        OrgService,
        {
          provide: BlockchainService,
          useClass: MockBlockchainService
        },
        {
          provide: ActivatedRoute,
          useValue: {
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                'add'
              ),
            },
          },
        },
      ]
    })
      .overrideModule(GridModule, {set: {
        imports: [
          FormsModule,
          MockSharedModule,
          RouterModule
        ],
      }})
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OrgsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
