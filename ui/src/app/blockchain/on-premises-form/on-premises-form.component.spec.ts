/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { OnPremisesFormComponent } from './on-premises-form.component';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { BlockchainService, MockBlockchainsService } from './../shared/blockchain.service';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';

describe('OnPremisesFormComponent', () => {
  let component: OnPremisesFormComponent;
  let fixture: ComponentFixture<OnPremisesFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [MockSharedModule],
      declarations: [
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
    fixture = TestBed.createComponent(OnPremisesFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
