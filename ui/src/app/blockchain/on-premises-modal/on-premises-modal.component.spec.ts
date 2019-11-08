/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { OnPremisesModalComponent } from './on-premises-modal.component';
import { RouterTestingModule } from '@angular/router/testing';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { BlockchainService, MockBlockchainsService } from './../shared/blockchain.service';

import { OnPremisesFormComponent } from '../on-premises-form/on-premises-form.component';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';

describe('OnPremisesModalComponent', () => {
  let component: OnPremisesModalComponent;
  let fixture: ComponentFixture<OnPremisesModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [MockSharedModule, RouterTestingModule],
      declarations: [
        OnPremisesModalComponent,
        OnPremisesFormComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent
      ],
      providers: [
        AuthenticationService,
        PersonaService,
        {
          provide: BlockchainService,
          useClass: MockBlockchainsService
        },
      ],

    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OnPremisesModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
