/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { LogDetailsComponent } from './log-details.component';


describe('LogDetailsComponent', () => {
  // let component: LogDetailsComponent;
  // let fixture: ComponentFixture<LogDetailsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule ],
      declarations: [ LogDetailsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    // fixture = TestBed.createComponent(LogDetailsComponent);
    // component = fixture.componentInstance;
    // fixture.detectChanges();
  });

  it('should create', () => {
    // expect(component).toBeTruthy();
  });
});
