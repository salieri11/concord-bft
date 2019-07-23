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
import { BlockchainService } from '../shared/blockchain.service';
import { MockSharedModule } from '../shared/shared.module';
import { OrgsComponent } from './orgs.component';

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
      ],
      providers: [
        OrgService,
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
        {
          provide: BlockchainService,
          useValue: {
            selectedBlockchain: {
              consortium_id: 1
            },
          },
        }

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
