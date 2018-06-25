/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { NodeDetailContainerComponent } from './node-detail-container.component';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { TransactionListViewComponent } from '../../transactions/transaction-list-view/transaction-list-view.component';
import { TransactionDetailsComponent } from '../../transactions/transaction-details/transaction-details.component';

describe('NodeDetailContainerComponent', () => {
  let component: NodeDetailContainerComponent;
  let fixture: ComponentFixture<NodeDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [
        NodeDetailContainerComponent,
        TransactionsStatusFilterComponent,
        TransactionListViewComponent,
        TransactionDetailsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodeDetailContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
