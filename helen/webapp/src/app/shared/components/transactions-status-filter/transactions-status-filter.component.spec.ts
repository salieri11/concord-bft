/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared.module';

import { TransactionsStatusFilterComponent } from './transactions-status-filter.component';

describe('TransactionsStatusFilterComponent', () => {
  let component: TransactionsStatusFilterComponent;
  let fixture: ComponentFixture<TransactionsStatusFilterComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule
      ],
      declarations: [
        TransactionsStatusFilterComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionsStatusFilterComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
