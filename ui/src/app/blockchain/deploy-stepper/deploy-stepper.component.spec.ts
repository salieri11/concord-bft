/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { of as observableOf } from 'rxjs';
import { DeployStepperComponent } from './deploy-stepper.component';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';
import { ZoneType } from '../../zones/shared/zones.model';
import { MockBlockchainService, BlockchainService } from '../shared/blockchain.service';
import { MainModule } from '../../main/main.module';
import { FeatureFlagService } from '../../shared/feature-flag.service';


describe('BlockchainWizardComponent', () => {
  let blockchainService: MockBlockchainService;
  const test = testFor(DeployStepperComponent).expedite({
    imports: [MainModule],
    provides: [FeatureFlagService],
    declarations: [],
  }, beforeTesting(() => {
    blockchainService = test.getService(BlockchainService);
  }), prepareEach(() => {}));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });

  describe('Detail page', () => {
    it('have a consortium name and description', () => {
      expect(test.component.form.get('details').value).toEqual({
        consortium_name: '',
        consortium_desc: ''
      });

      const value = {
        consortium_name: 'Test',
        consortium_desc: 'test'
      };
      test.component.form.controls.details.setValue(value);

      expect(test.component.form.get('details').value).toEqual(value);
    });
  });

  describe('Distribute regions', () => {
    it('should be evenly distributed', () => {
      test.component.zones = [{
        name: 'US West - Oregon',
        id: 'us-west',
        latitude: 0,
        longitude: 0,
        type: ZoneType.VMC_AWS
      }, {
        name: 'US East - N Virginia',
        id: 'us-east',
        latitude: 0,
        longitude: 0,
        type: ZoneType.VMC_AWS
      }, {
        name: 'EMEA - Frankfurt',
        id: 'emea',
        latitude: 0,
        longitude: 0,
        type: ZoneType.VMC_AWS
      },
      {
        name: 'Pacific - Sydney',
        id: 'pacific',
        latitude: 0,
        longitude: 0,
        type: ZoneType.VMC_AWS
      }];
      test.component.onPremActive = false;
      test.component.zoneTabSelect(ZoneType.VMC_AWS);
      const zones = test.component.form.controls.nodes['controls'].zones;
      const regionKeys = Object.keys(zones.value);

      test.component.form.controls.nodes['controls'].numberOfNodes.patchValue(7);
      test.component.distributeZones();

      expect(zones.controls[regionKeys[0]].value).toEqual(2);
      expect(zones.controls[regionKeys[1]].value).toEqual(2);
      expect(zones.controls[regionKeys[2]].value).toEqual(2);
      expect(zones.controls[regionKeys[3]].value).toEqual(1);
    });
  });

  describe('On submit', () => {

    beforeEach(() => {
      spyOn(blockchainService, 'deploy')
          .and.returnValue(observableOf({}));
    });

    it('submits the form value', () => {
      test.component.form.controls.nodes['controls'].numberOfNodes.patchValue(4);

      test.component.onSubmit();
      // TODO: Test the individual attributes of the request
      expect(blockchainService.deploy).toHaveBeenCalled();
    });
  });
});
