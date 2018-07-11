/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';

import { BlockchainWizardComponent } from './blockchain-wizard.component';
import { MockSharedModule } from '../../shared.module';
import { VmwComboboxComponent } from '../combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../accordion/accordion-group.component';
import { VmwAccordionComponent } from '../accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../combobox/combobox-items/combobox-items.component';

describe('BlockchainWizardComponent', () => {
  let component: BlockchainWizardComponent;
  let fixture: ComponentFixture<BlockchainWizardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        MockSharedModule,
        FormsModule
      ],
      declarations: [
        BlockchainWizardComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent,
        VmwAccordionGroupComponent,
        VmwAccordionComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainWizardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
