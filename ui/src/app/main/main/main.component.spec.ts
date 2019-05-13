/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { TourService as NgxTourService } from 'ngx-tour-ngx-popper';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ActivatedRoute } from '@angular/router';

import { MockSharedModule } from '../../shared/shared.module';

import { MainComponent } from './main.component';
import { AppHeaderComponent } from '../../shared/components/app-header/app-header.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { VmwTaskComponent } from '../../shared/components/task-panel/task.component';
import { VmwThemeSwitchButtonComponent } from '../../shared/components/theme-switch-button/theme-switch-button.component';
import { BlockchainWizardComponent } from '../../shared/components/blockchain-wizard/blockchain-wizard.component';
import { VersionComponent } from '../../shared/components/version/version.component';
import { DeployingInterstialComponent } from '../deploying-interstitial/deploying-interstitial.component';
import { SetupModalComponent } from '../setup-modal/setup-modal.component';

import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { AuthenticationService } from '../../shared/authentication.service';
import { Personas } from '../../shared/persona.service';
import { VmwClarityThemeService } from '../../shared/theme.provider';
import { VmwTasksService } from '../../shared/components/task-panel/tasks.service';
import { TourService } from '../../shared/tour.service';
import { BlockchainService } from '../../shared/blockchain.service';


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
        SetupModalComponent,
        BlockchainWizardComponent,
      ],
      providers: [
        ErrorAlertService,
        VmwTasksService,
        TourService,
        NgxTourService,
        VmwClarityThemeService,
        BlockchainService,
        {
          provide: ActivatedRoute,
          useValue: {
            snapshot: {params: {consortiumId: '1234'}},
            params: {
              subscribe: (fn: (value) => void) => fn(
                ''
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
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('when authenticated', () => {
    beforeEach(() => {
      (TestBed.get(AuthenticationService) as AuthenticationService).logIn('test@vmware.com', 'asdfasdf', Personas.SystemsAdmin);
    });
    afterEach(() => {
      (TestBed.get(AuthenticationService) as AuthenticationService).logOut();
    });

    it('should render a nav bar title', async(() => {
      const testFixture = TestBed.createComponent(MainComponent);
      testFixture.detectChanges();
      const compiled = testFixture.debugElement.nativeElement;
      expect(compiled.querySelector('.branding .title').textContent).toContain('title');
    }));
  });
});
