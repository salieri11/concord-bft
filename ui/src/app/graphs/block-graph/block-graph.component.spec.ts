/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { NgxChartsModule } from '@swimlane/ngx-charts';

import { BlockGraphComponent } from './block-graph.component';

describe('BlockGraphComponent', () => {
  let component: BlockGraphComponent;
  let fixture: ComponentFixture<BlockGraphComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ NgxChartsModule, BrowserAnimationsModule ],
      declarations: [ BlockGraphComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockGraphComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
