/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ClarityModule } from '@clr/angular';
import { MockTranslateModule } from '../../mocks/mock-translate.module';
import { SharedModule } from '../../shared/shared.module';

import { NodeDetailContainerComponent } from './node-detail-container.component';
import { TransactionFiltersModalComponent } from '../transaction-filters-modal/transaction-filters-modal.component';

describe('NodeDetailContainerComponent', () => {
  let component: NodeDetailContainerComponent;
  let fixture: ComponentFixture<NodeDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        ClarityModule,
        MockTranslateModule,
        SharedModule
      ],
      declarations: [ NodeDetailContainerComponent, TransactionFiltersModalComponent ]
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
