/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClarityModule } from '@clr/angular';

import { NodeDetailContainerComponent } from './node-detail-container.component';
import { TransactionFiltersModalComponent } from '../transaction-filters-modal/transaction-filters-modal.component';

describe('NodeDetailContainerComponent', () => {
  let component: NodeDetailContainerComponent;
  let fixture: ComponentFixture<NodeDetailContainerComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClarityModule
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
