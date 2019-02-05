/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { NgxChartsModule } from '@swimlane/ngx-charts';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';

import { LogGraphComponent } from './log-graph.component';
import { MockSharedModule } from '../../shared/shared.module';

describe('LogGraphComponent', () => {
  let component: LogGraphComponent;
  let fixture: ComponentFixture<LogGraphComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule, NgxChartsModule, NoopAnimationsModule ],
      declarations: [ LogGraphComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LogGraphComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
