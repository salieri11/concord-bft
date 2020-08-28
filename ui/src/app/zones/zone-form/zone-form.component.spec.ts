/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { MockSharedModule } from '../../shared/shared.module';
import { ZoneFormComponent } from './zone-form.component';

import { AuthenticationService } from '../../shared/authentication.service';
import { PersonaService } from '../../shared/persona.service';
import { BlockchainService, MockBlockchainService } from './../../blockchain/shared/blockchain.service';
import { VmwComboboxComponent } from '../../shared/components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from '../../shared/components/combobox/combobox-items/combobox-items.component';

describe('ZoneFormComponent', () => {
  let component: ZoneFormComponent;
  let fixture: ComponentFixture<ZoneFormComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [MockSharedModule],
      declarations: [
        ZoneFormComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent
      ],
      providers: [
        AuthenticationService,
        PersonaService,
        {
          provide: BlockchainService,
          useClass: MockBlockchainService
        },
      ],

    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ZoneFormComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
