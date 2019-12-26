/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TransactionListDamlComponent } from './transaction-list-daml.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';

describe('TransactionListDamlComponent', () => {
  let component: TransactionListDamlComponent;
  let fixture: ComponentFixture<TransactionListDamlComponent>;

  beforeEach(async( () => {
    const tester = getSpecTestingModule();
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TransactionListDamlComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
