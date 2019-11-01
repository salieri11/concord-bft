/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { NgxChartsModule } from '@swimlane/ngx-charts';

import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute } from '@angular/router';
import { MockSharedModule } from '../../shared/shared.module';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';

import { DashboardComponent } from './dashboard.component';
import { TransactionsStatusFilterComponent } from '../../shared/components/transactions-status-filter/transactions-status-filter.component';
import { TransactionListComponent } from '../../transactions/transaction-list/transaction-list.component';
import { TransactionDetailsComponent } from '../../transactions/transaction-details/transaction-details.component';
import { BlockchainWizardComponent } from '../../blockchain/blockchain-wizard/blockchain-wizard.component';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../../shared/components/accordion/accordion-group.component';
import { VmwAccordionComponent } from '../../shared/components/accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { DashboardListComponent } from '../dashboard-list/dashboard-list.component';

import { WorldMapComponent } from '../../graphs/world-map/world-map.component';
import { TourService } from '../../shared/tour.service';
import { BlockchainService, BlockchainsServiceMock } from '../../blockchain/shared/blockchain.service';
import { VmwClarityThemeService } from './../../shared/theme.provider';

import { OnPremisesFormComponent } from '../../blockchain/on-premises-form/on-premises-form.component';
import { OnPremisesModalComponent } from '../../blockchain/on-premises-modal/on-premises-modal.component';
import { FeatureFlagDirective } from '../../shared/directives/feature-flag.directive';


describe('DashboardComponent', () => {
  let component: DashboardComponent;
  let fixture: ComponentFixture<DashboardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule,
        FormsModule,
        NgxChartsModule,
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
        VmwAccordionGroupComponent,
        VmwCopyToClipboardButtonComponent,
        DashboardListComponent,
        OnPremisesFormComponent,
        OnPremisesModalComponent,
        FeatureFlagDirective,
      ],
      providers: [
        VmwTasksService,
        TourService,
        NgxTourService,
        VmwClarityThemeService,
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: { parent: { parent: { params: { consortiumId: '1234' } } } },
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                {}
              ),
            },
          },
        },
        {
          provide: BlockchainService,
          useClass: BlockchainsServiceMock
        },
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
