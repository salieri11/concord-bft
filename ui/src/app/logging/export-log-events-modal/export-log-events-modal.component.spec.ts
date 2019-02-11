/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { ExportLogEventsModalComponent } from './export-log-events-modal.component';
import { ErrorAlertService } from '../../shared/global-error-handler.service';

describe('ExportLogEventsModalComponent', () => {
  let component: ExportLogEventsModalComponent;
  let fixture: ComponentFixture<ExportLogEventsModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ HttpClientTestingModule, MockSharedModule ],
      declarations: [ ExportLogEventsModalComponent ],
      providers: [ ErrorAlertService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ExportLogEventsModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
