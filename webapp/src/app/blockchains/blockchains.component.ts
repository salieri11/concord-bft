/*
 * Copyright 2018 VMware, all rights reserved.
 */
import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';
import {
  FormBuilder,
  FormGroup,
  Validators
} from '@angular/forms';
import { ActivatedRoute } from '@angular/router';

import { TranslateService } from '@ngx-translate/core';

import { GridOptions } from '../grid/shared/grid.model';
import { GridComponent } from '../grid/grid.component';
import { Blockchain } from './shared/blockchains.model';
import { BlockchainsService } from './shared/blockchains.service';
import { KubernetesService } from '../kubernetes-management/shared/kubernetes.service';
import { OrgManagementService } from '../org-management/shared/org-management.service';
import { Personas } from '../shared/persona.service';

@Component({
  selector: 'athena-blockchains',
  templateUrl: './blockchains.component.html',
  styleUrls: ['./blockchains.component.scss']
})
export class BlockchainsComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('grid') grid: GridComponent;
  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  addBlockchainForm: FormGroup;
  gridOptions: GridOptions = new GridOptions();

  selectedRows: Array<Blockchain>;
  consensusTypes: Array<string> = ['SOLO', 'KAFKA'];
  ordererOrgs: any = [];
  orgs: any = [];
  kubes: any = [];

  constructor(
    private blockchainsService: BlockchainsService,
    private orgService: OrgManagementService,
    private kubeService: KubernetesService,
    private fb: FormBuilder,
    private translate: TranslateService,
    private route: ActivatedRoute
  ) {
    this.gridOptions.getData = (params?: any) => {
      return this.blockchainsService.getList(params);
    };

    this.handleGrid();
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'add':
          this.createAddBlockchainForm();
          this.openAddBlockchain();
          break;

        default:
          // code...
          break;
      }
    });
  }

  selectedRowChange(rows: Array<Blockchain>): void {
    this.selectedRows = rows;
  }

  addBlockchain(): void {
    const formModel = this.addBlockchainForm.value;
    const orgs = formModel.peerOrg.map(id => {
        return { id: id.toString() };
      });
    orgs.push({id: formModel.ordererOrg });

    const blockchain = {
      name: formModel.name,
      organizations: orgs,
      consensusType: formModel.consensusType,
      kubernetesBlockchain: { id: formModel.kubernetes },
      cli: formModel.createCli,
    };

    this.blockchainsService.create(blockchain)
      .subscribe(response => this.handleBlockchainAdd(response));
  }

  openAddBlockchain(): void {
    this.openModal('add');
    this.getBlockchainFormData();
    this.createAddBlockchainForm();
  }

  confirmDeleteBlockchain(): void {
    this.openModal('delete');
  }

  deleteBlockchain(): void {
    this.selectedRows.forEach(org => {
      this.blockchainsService.delete(org.id)
        .subscribe(response => this.handleBlockchainDeletion(response));
    });
  }


  private handleBlockchainAdd(response): void {
    this.openModalForm = false;
    this.grid.addRow(response);
  }

  private handleBlockchainDeletion(response): void {
    this.openModalForm = false;
    this.grid.reload();
    console.log('Blockchain Delete', response);
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

  private handleGrid(): void {
    this.gridOptions.paginationTitle = this.translate.instant('blockchains.grid.pagination.title');

    this.gridOptions.columns = [{
      id: 'name',
      name: this.translate.instant('blockchains.grid.columns.name.title'),
      type: 'link',
      genLink: (row: Blockchain) => {
        return `/blockchains/${row.id}`;
      }
    }, {
      id: 'consensusType',
      name: this.translate.instant('blockchains.grid.columns.consensusType.title'),
      type: 'string'
    }, {
      id: 'state',
      name: this.translate.instant('blockchains.grid.columns.status.title'),
      type: 'string'
    }, {
      id: 'lastAction',
      name: this.translate.instant('blockchains.grid.columns.action.title'),
      type: 'string'
    }, {
      id: 'k8sDashboardUrl',
      name: this.translate.instant('blockchains.grid.columns.k8sDashboardUrl.title'),
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.k8sDashboardUrl;
      }
    }, {
      id: 'fabricExplorerUrl',
      name: this.translate.instant('blockchains.grid.columns.fabricExplorerUrl.title'),
      type: 'externalLink',
      genLink: (row: Blockchain) => {
        return row.fabricExplorerUrl;
      }
    }, {
      id: 'createdOn',
      name: this.translate.instant('blockchains.grid.columns.created.title'),
      type: 'date'
    }
    ];
  }

}
