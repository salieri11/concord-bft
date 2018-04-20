/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ClarityModule } from '@clr/angular';
import { MockTranslateModule } from '../../mocks/mock-translate.module';

import { TransactionDetailContainerComponent } from './transaction-detail-container.component';
import { SharedModule } from '../../shared/shared.module';

describe('TransactionDetailContainerComponent', () => {
  let component: TransactionDetailContainerComponent;
  let fixture: ComponentFixture<TransactionDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        ClarityModule,
        MockTranslateModule,
        SharedModule
      ],
      declarations: [ TransactionDetailContainerComponent ]
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
