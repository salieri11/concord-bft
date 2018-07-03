/*
 * Copyright 2018 VMware, all rights reserved.
 */
import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { Blockchain } from './shared/blockchains.model';
import { Personas } from '../shared/persona.service';
import { BlockchainsListComponent } from './blockchains-list/blockchains-list.component';
import { BlockchainFormComponent } from './blockchain-form/blockchain-form.component';

@Component({
  selector: 'athena-blockchains',
  templateUrl: './blockchains.component.html',
  styleUrls: ['./blockchains.component.scss']
})
export class BlockchainsComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('blockchainsList') blockchainsList: BlockchainsListComponent;
  @ViewChild('blockchainsForm') blockchainsForm: BlockchainFormComponent;

  selected: Array<Blockchain>;

  constructor() {
  }

  ngOnInit() {
  }

  blockchainsSelectionChange(rows: Array<Blockchain>): void {
    this.selected = rows;
  }

  addBlockchains(blockchain: Blockchain) {
    this.blockchainsList.grid.addRow(blockchain);
  }

  deleteBlockchains() {
    this.blockchainsList.grid.reload();
  }


}
