/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { AthenaApiService } from '../../shared/athena-api.service';
import { ContractFormComponent } from '../contract-form/contract-form.component';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'app-smart-contracts-container',
  templateUrl: './smart-contracts-container.component.html',
  styleUrls: ['./smart-contracts-container.component.scss']
})
export class SmartContractsContainerComponent implements OnInit {
  @ViewChild('contractFormModal') contractFormModal: ContractFormComponent;

  smartContracts = [];
  personas = Personas;

  constructor( private athenaApiService: AthenaApiService ) { }

  ngOnInit() {
    this.loadSmartContracts();
  }

  loadSmartContracts() {
    this.athenaApiService.getSmartContracts().subscribe(smartContracts => this.smartContracts = smartContracts);
  }
}
