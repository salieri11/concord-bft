/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { of as observableOf } from 'rxjs';
import { BlockchainWizardComponent } from './blockchain-wizard.component';
import { MockSharedModule } from '../../shared.module';
import { VmwComboboxComponent } from '../combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../accordion/accordion-group.component';
import { VmwAccordionComponent } from '../accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../combobox/combobox-items/combobox-items.component';
import { BlockchainService, BlockchainsServiceMock } from '../../blockchain.service';

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
      ],
      providers: [{provide: BlockchainService, useClass: BlockchainsServiceMock}]
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

  describe('Detail page', () => {
    it('have a consortium name and description', () => {
      expect(component.form.get('details').value).toEqual({
        consortium_name: '',
        consortium_desc: ''
      });

      const value = {
        consortium_name: 'Test',
        consortium_desc: 'test'
      };
      component.form.controls.details.setValue(value);

      expect(component.form.get('details').value).toEqual(value);
    });
  });

  describe('Distribute regions', () => {
    it('regions should be evenly distributed', () => {
      component.zones = [{
        name: 'US West - Oregon',
        id: 'us-west',
        latitude: 0,
        longitude: 0
      }, {
        name: 'US East - N Virginia',
        id: 'us-east',
        latitude: 0,
        longitude: 0
      }, {
        name: 'EMEA - Frankfurt',
        id: 'emea',
        latitude: 0,
        longitude: 0
      },
      {
        name: 'Pacific - Sydney',
        id: 'pacific',
        latitude: 0,
        longitude: 0
      }];

      const zones = component.form.controls.nodes['controls'].zones;
      const regionKeys = Object.keys(zones.value);

      component.form.controls.nodes['controls'].numberOfNodes.patchValue(7);
      component.distributeZones();

      expect(zones.controls[regionKeys[0]].value).toEqual(2);
      expect(zones.controls[regionKeys[1]].value).toEqual(2);
      expect(zones.controls[regionKeys[2]].value).toEqual(2);
      expect(zones.controls[regionKeys[3]].value).toEqual(1);
    });
  });

  describe('On open', () => {
    it('resets all forms', () => {
      spyOn(component.form, 'reset');

      component.open();

      expect(component.form.reset).toHaveBeenCalled();
    });

    it('resets the clarity wizard', () => {
      spyOn(component.wizard, 'reset');

      component.open();

      expect(component.wizard.reset).toHaveBeenCalled();
    });

    it('sets isOpen', () => {
      expect(component.isOpen).toBe(false);

      component.open();

      expect(component.isOpen).toBe(true);
    });
  });

  describe('On submit', () => {

    beforeEach(() => {
      spyOn((component as any).blockchainService, 'deploy')
          .and.returnValue(observableOf({}));
    });

    it('submits the form value', () => {
      spyOn((component as any).router, 'navigate');
      component.form.controls.nodes['controls'].numberOfNodes.patchValue(4);

      component.onSubmit();
      // TODO: Test the individual attributes of the request
      expect((component as any).blockchainService.deploy).toHaveBeenCalled();
    });
  });
});
