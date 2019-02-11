/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { ExportChartDataModalComponent } from './export-chart-data-modal.component';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

describe('ExportChartDataModalComponent', () => {
  let component: ExportChartDataModalComponent;
  let fixture: ComponentFixture<ExportChartDataModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule ],
      declarations: [ ExportChartDataModalComponent ],
      providers: [ ErrorAlertService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ExportChartDataModalComponent);
    component = fixture.componentInstance;
    component.chartData = [];
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
