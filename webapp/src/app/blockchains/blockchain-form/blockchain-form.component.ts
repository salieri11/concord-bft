/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

import { KubernetesService } from '../../kubernetes/shared/kubernetes.service';
import { Blockchain } from '../shared/blockchains.model';
import { BlockchainsService } from '../shared/blockchains.service';
import { OrgService } from '../../orgs/shared/org.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-blockchain-form',
  templateUrl: './blockchain-form.component.html',
  styleUrls: ['./blockchain-form.component.scss']
})
export class BlockchainFormComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @Input('selected') selected: Array<Blockchain>;
  @Output() addBlockchains: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteBlockchains: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  addBlockchainForm: FormGroup;

  consensusTypes: Array<string> = ['SOLO', 'KAFKA'];
  ordererOrgs: any = [];
  orgs: any = [];
  kubes: any = [];

  constructor(
    private translate: TranslateService,
    private blockchainsService: BlockchainsService,
    private orgService: OrgService,
    private kubeService: KubernetesService,
    private fb: FormBuilder,
    private route: ActivatedRoute
  ) { }

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
    this.selected = rows;
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
    this.selected.forEach(org => {
      this.blockchainsService.delete(org.id)
        .subscribe(response => this.handleBlockchainDeletion(response));
    });
  }


  private handleBlockchainAdd(response): void {
    this.openModalForm = false;
    this.addBlockchains.emit(response);
  }

  private handleBlockchainDeletion(response): void {
    this.openModalForm = false;
    console.log('Blockchain Delete', response);
    this.deleteBlockchains.emit();
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

}
