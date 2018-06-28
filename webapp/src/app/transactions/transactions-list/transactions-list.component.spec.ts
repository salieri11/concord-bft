/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { TransactionsListComponent } from './transactions-list.component';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { TransactionDetailsComponent } from '../transaction-details/transaction-details.component';

describe('TransactionsListComponent', () => {
  let component: TransactionsListComponent;
  let fixture: ComponentFixture<TransactionsListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [
        TransactionsListComponent,
        TransactionsStatusFilterComponent,
        TransactionDetailsComponent
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
