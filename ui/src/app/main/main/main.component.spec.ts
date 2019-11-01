/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import { MockSharedModule } from '../../shared/shared.module';

import { MainComponent } from './main.component';
import { AppHeaderComponent } from '../../shared/components/app-header/app-header.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { VmwTaskComponent } from '../../shared/components/task-panel/task.component';
import { VmwThemeSwitchButtonComponent } from '../../shared/components/theme-switch-button/theme-switch-button.component';
import { BlockchainWizardComponent } from '../../blockchain/blockchain-wizard/blockchain-wizard.component';
import { VersionComponent } from '../../shared/components/version/version.component';
import { DeployingInterstialComponent } from '../deploying-interstitial/deploying-interstitial.component';
import { WelcomeComponent } from '../welcome/welcome.component';

import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { VmwClarityThemeService } from '../../shared/theme.provider';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { TourService } from '../../shared/tour.service';
import { BlockchainService, BlockchainsServiceMock } from '../../blockchain/shared/blockchain.service';

import { InivteUserComponent } from '../../orgs/inivte-user/inivte-user.component';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';
import { OnPremisesModalComponent } from '../../blockchain/on-premises-modal/on-premises-modal.component';
import { OnPremisesFormComponent } from '../../blockchain/on-premises-form/on-premises-form.component';
import { FeatureFlagDirective } from '../../shared/directives/feature-flag.directive';


describe('MainComponent', () => {
  let component: MainComponent;
  let fixture: ComponentFixture<MainComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        HttpClientTestingModule
      ],
      declarations: [
        MainComponent,
        VmwTaskComponent,
        CanViewDirective,
        AppHeaderComponent,
        VmwThemeSwitchButtonComponent,
        VersionComponent,
        DeployingInterstialComponent,
        WelcomeComponent,
        BlockchainWizardComponent,
        InivteUserComponent,
        OnPremisesModalComponent,
        OnPremisesFormComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent,
        FeatureFlagDirective
      ],
      providers: [
        ErrorAlertService,
        VmwTasksService,
        TourService,
        NgxTourService,
        VmwClarityThemeService,
        {
          provide: BlockchainService,
          useClass: BlockchainsServiceMock
        },
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: { params: { consortiumId: '1234' } },
            data: {
              subscribe: (fn: (value) => void) => fn(
                { blockchains: [] }
              ),
            },
            params: {
              subscribe: (fn: (value) => void) => fn(
                { consortiumId: 1 }
              ),
            },
            fragment: {
              subscribe: (fn: (value) => void) => fn(
                ''
              ),
            },
          },
        }
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MainComponent);
    component = fixture.componentInstance;
    component.routerFragmentChange = new Subscription();
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

});
