/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { NgxChartsModule } from '@swimlane/ngx-charts';

import { LogHeatMapComponent } from './log-heat-map.component';

describe('LogHeatMapComponent', () => {
  let component: LogHeatMapComponent;
  let fixture: ComponentFixture<LogHeatMapComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ NgxChartsModule, NoopAnimationsModule ],
      declarations: [ LogHeatMapComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(LogHeatMapComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
