/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ClrFormsNextModule } from '@clr/angular';

import { UserSettingsComponent } from './user-settings.component';
import { MockSharedModule } from '../../shared/shared.module';
import { TourService } from '../../shared/tour.service';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { WalletFormComponent } from '../wallet-form/wallet-form.component';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { AuthenticationService } from '../../shared/authentication.service';

describe('UserSettingsComponent', () => {
  let component: UserSettingsComponent;
  let fixture: ComponentFixture<UserSettingsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        MockSharedModule,
        ClrFormsNextModule,
        RouterTestingModule
      ],
      declarations: [
        UserSettingsComponent,
        WalletFormComponent,
        VmwCopyToClipboardButtonComponent
      ],
      providers: [
        TourService,
        ErrorAlertService,
        {
          provide: AuthenticationService,
          useValue: {
            currentUser: {
              wallet_address: ''
            }
          }}
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(UserSettingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
