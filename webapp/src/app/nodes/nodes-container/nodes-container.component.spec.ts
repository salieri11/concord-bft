/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ClarityModule } from '@clr/angular';
import { FormsModule } from '@angular/forms';
import { MockTranslateModule } from '../../mocks/mock-translate.module';
import { SharedModule } from '../../shared/shared.module';

import { NodesContainerComponent } from './nodes-container.component';
import { TransactionFiltersModalComponent } from '../transaction-filters-modal/transaction-filters-modal.component';
import { NodesStatusFilterComponent } from '../nodes-status-filter/nodes-status-filter.component';

describe('NodesContainerComponent', () => {
  let component: NodesContainerComponent;
  let fixture: ComponentFixture<NodesContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        ClarityModule,
        MockTranslateModule,
        FormsModule,
        SharedModule
      ],
      declarations: [ NodesContainerComponent, TransactionFiltersModalComponent, NodesStatusFilterComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NodesContainerComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
