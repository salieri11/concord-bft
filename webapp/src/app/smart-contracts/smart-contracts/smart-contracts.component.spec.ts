/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { of as observableOf } from 'rxjs';

import { MockSharedModule } from '../../shared/shared.module';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsComponent } from './smart-contracts.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { TourService } from '../../shared/tour.service';
import { SmartContract } from '../shared/smart-contracts.model';

describe('SmartContractsComponent', () => {
  let component: SmartContractsComponent;
  let fixture: ComponentFixture<SmartContractsComponent>;
  let smartContracts: SmartContract[];
  let getSpy;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule,
        MockSharedModule
      ],
      declarations: [ SmartContractsComponent, ContractFormComponent, CanViewDirective ],
      providers: [ TourService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SmartContractsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    smartContracts = [{
      contract_id: 'id',
      owner: 'owner',
      url: 'url',
      versions: [{
        address: 'address',
        metadata: {},
        version: 'version',
        url: 'url',
      }]
    }];
    getSpy = spyOn((component as any).smartContractsService, 'getSmartContracts')
      .and.returnValue(observableOf(smartContracts));
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('On init', () => {
    it('loads smart contracts', () => {
      component.ngOnInit();

      expect(getSpy).toHaveBeenCalled();
    });
  });

  describe('On load', () => {
    it('sets smartContracts as the API response', () => {
      component.loadSmartContracts();

      expect(component.smartContracts).toEqual(smartContracts);
    });
  });
});
