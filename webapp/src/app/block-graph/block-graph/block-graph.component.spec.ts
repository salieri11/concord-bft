/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockGraphComponent } from './block-graph.component';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

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
