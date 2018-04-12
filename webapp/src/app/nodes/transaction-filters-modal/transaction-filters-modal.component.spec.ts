/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { ClarityModule } from '@clr/angular';

import { TransactionFiltersModalComponent } from './transaction-filters-modal.component';
import { MockTranslateModule } from '../../mocks/mock-translate.module';

describe('TransactionFiltersModalComponent', () => {
  let component: TransactionFiltersModalComponent;
  let fixture: ComponentFixture<TransactionFiltersModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ ClarityModule, MockTranslateModule ],
      declarations: [ TransactionFiltersModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionFiltersModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
