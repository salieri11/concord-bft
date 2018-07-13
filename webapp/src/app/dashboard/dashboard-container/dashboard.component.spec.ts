/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockSharedModule } from '../../shared/shared.module';

import { DashboardComponent } from './dashboard.component';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { TransactionListComponent } from '../../transactions/transaction-list/transaction-list.component';
import { TransactionDetailsComponent } from '../../transactions/transaction-details/transaction-details.component';
import { BlockchainWizardComponent } from '../../shared/components/blockchain-wizard/blockchain-wizard.component';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../../shared/components/accordion/accordion-group.component';
import { VmwAccordionComponent } from '../../shared/components/accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { WorldMapComponent } from '../world-map/world-map.component';

describe('DashboardComponent', () => {
  let component: DashboardComponent;
  let fixture: ComponentFixture<DashboardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule,
        FormsModule
      ],
      declarations: [
        DashboardComponent,
        WorldMapComponent,
        TransactionsStatusFilterComponent,
        TransactionListComponent,
        TransactionDetailsComponent,
        BlockchainWizardComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent,
        VmwAccordionComponent,
        VmwAccordionGroupComponent
      ],
      providers: [
        VmwTasksService
      ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DashboardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
