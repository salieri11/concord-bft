/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';

import { BlockchainSetupWizardComponent } from './blockchain-setup-wizard.component';
import { MockSharedModule } from '../../shared.module';
import { VmwComboboxComponent } from '../combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../accordion/accordion-group.component';
import { VmwAccordionComponent } from '../accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../combobox/combobox-items/combobox-items.component';

describe('BlockchainSetupWizardComponent', () => {
  let component: BlockchainSetupWizardComponent;
  let fixture: ComponentFixture<BlockchainSetupWizardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        FormsModule
      ],
      declarations: [
        BlockchainSetupWizardComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent,
        VmwAccordionGroupComponent,
        VmwAccordionComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainSetupWizardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
