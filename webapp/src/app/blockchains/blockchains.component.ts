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

  private getBlockchainFormData(): void {
    this.orgService.getOrdererOrgs()
      .subscribe(ordererOrgs => {
        this.ordererOrgs = ordererOrgs.objects;
      });
    this.orgService.getUsableOrgs()
      .subscribe(orgs => {
        this.orgs = orgs.objects;
      });

    this.kubeService.getList()
      .subscribe(kubes => {
        this.kubes = kubes.objects;
      });
  }

  private createAddBlockchainForm() {
    this.addBlockchainForm = this.fb.group({
      name: ['', Validators.required],
      peerOrg: ['', Validators.required],
      consensusType: ['', Validators.required],
      ordererOrg: ['', Validators.required],
      kubernetes: ['', Validators.required],
      createCli: [false, Validators.required],
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.modalSize = 'md';
        this.translate.get('blockchains.addBlockchainForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('blockchains.deleteBlockchainForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;

  }

  private handleGrid(grid: any): void {
    this.gridOptions.paginationTitle = grid.pagination.title;

    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'link',
      genLink: (row: Blockchain) => {
        return `/blockchains/${row.id}`;
      }
    }, {
      id: 'consensusType',
      name: grid.columns.consensusType.title,
      type: 'string'
    }, {
      id: 'state',
      name: grid.columns.status.title,
      type: 'string'
    }, {
      id: 'lastAction',
      name: grid.columns.action.title,
      type: 'string'
    }, {
      id: 'k8sDashboardUrl',
      name: grid.columns.k8sDashboardUrl.title,
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.k8sDashboardUrl;
      }
    }, {
      id: 'fabricExplorerUrl',
      name: grid.columns.fabricExplorerUrl.title,
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.fabricExplorerUrl;
      }
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }
    ];
  }

}
