/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { EnviornmentComponent } from './enviornment.component';

describe('EnviornmentComponent', () => {
  let component: EnviornmentComponent;
  let fixture: ComponentFixture<EnviornmentComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ EnviornmentComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(EnviornmentComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
