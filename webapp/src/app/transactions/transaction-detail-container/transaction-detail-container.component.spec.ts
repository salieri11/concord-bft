/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { MockSharedModule } from '../../shared/shared.module';
import { TransactionDetailContainerComponent } from './transaction-detail-container.component';
import { TransactionDetailsComponent } from '../transaction-details/transaction-details.component';

describe('TransactionDetailContainerComponent', () => {
  let component: TransactionDetailContainerComponent;
  let fixture: ComponentFixture<TransactionDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [
        TransactionDetailContainerComponent,
        TransactionDetailsComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionDetailContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
