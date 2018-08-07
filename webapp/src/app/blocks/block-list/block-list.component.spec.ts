/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { APP_BASE_HREF } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { MockSharedModule } from '../../shared/shared.module';
import { BlockDetailsComponent } from '../block-details/block-details.component';
import { BlockListComponent } from './block-list.component';
import { BlockGraphComponent } from '../../block-graph/block-graph/block-graph.component';

describe('BlockListComponent', () => {
  let component: BlockListComponent;
  let fixture: ComponentFixture<BlockListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule,
        NgxChartsModule,
        BrowserAnimationsModule
      ],
      declarations: [
        BlockListComponent,
        BlockDetailsComponent,
        BlockGraphComponent
      ],
      providers: [{provide: APP_BASE_HREF, useValue: '/'}]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
