/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { APP_BASE_HREF } from '@angular/common';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { MockSharedModule } from '../../shared/shared.module';
import { NodesStatusFilterComponent } from '../nodes-status-filter/nodes-status-filter.component';
import { NodesComponent } from './nodes.component';
import { NodeListComponent } from '../node-list/node-list.component';
import { BlockGraphComponent } from '../../graphs/block-graph/block-graph.component';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';

describe('NodesComponent', () => {
  let component: NodesComponent;
  let fixture: ComponentFixture<NodesComponent>;

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
        NodesComponent,
        NodesStatusFilterComponent,
        BlockGraphComponent,
        NodeListComponent,
        VmwCopyToClipboardButtonComponent
      ],
      providers: [{provide: APP_BASE_HREF, useValue: '/'}]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
