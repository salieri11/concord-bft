/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { ContractFormComponent } from '../contract-form/contract-form.component';
import { SmartContractsService } from '../shared/smart-contracts.service';
import { Personas } from '../../shared/persona.service';
import { TourService } from '../../shared/tour.service';

@Component({
  selector: 'athena-smart-contracts',
  templateUrl: './smart-contracts.component.html',
  styleUrls: ['./smart-contracts.component.scss']
})
export class SmartContractsComponent implements OnInit {
  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;

  smartContracts = [];
  personas = Personas;

  constructor( private smartContractsService: SmartContractsService, private tourService: TourService) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  loadSmartContracts() {
    this.smartContractsService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }

  onPrev() {
    this.tourService.scrollToTransactionList();
  }
}
