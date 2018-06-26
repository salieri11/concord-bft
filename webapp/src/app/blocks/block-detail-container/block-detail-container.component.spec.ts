/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { BlockDetailContainerComponent } from './block-detail-container.component';
import { TransactionListViewComponent } from '../../transactions/transaction-list-view/transaction-list-view.component';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { TransactionDetailsComponent } from '../../transactions/transaction-details/transaction-details.component';

describe('BlockDetailContainerComponent', () => {
  let component: BlockDetailContainerComponent;
  let fixture: ComponentFixture<BlockDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [
        BlockDetailContainerComponent,
        TransactionListViewComponent,
        TransactionsStatusFilterComponent,
        TransactionDetailsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockDetailContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
