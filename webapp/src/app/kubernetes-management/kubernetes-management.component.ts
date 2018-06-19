/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  ChangeDetectorRef,
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
import { KubernetesService } from './shared/kubernetes.service';
import { Kubernetes, CredType } from './shared/kubernetes.model';
import { Personas } from '../shared/persona.service';


@Component({
  selector: 'app-kubernetes-management',
  templateUrl: './kubernetes-management.component.html',
  styleUrls: ['./kubernetes-management.component.scss']
})
export class KubernetesManagementComponent implements OnInit {
  static personasAllowed: string[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('grid') grid: GridComponent;
  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  gridOptions: GridOptions = new GridOptions();

  addKubeForm: FormGroup;
  get credentialType(): any { return this.addKubeForm.get('credentialType'); }
  deleteKubeForm: FormGroup;
  selectedRows: Array<Kubernetes>;
  credType = CredType;
  credentialOptions: Array<{ name?: string; value: string }> = [
    { value: CredType.BasicAuth, name: 'Basic Auth' },
    { value: CredType.Certificate, name: 'Certificate' },
    { value: CredType.ConfigFile, name: 'Config File' },
  ];

  constructor(
    private kubeService: KubernetesService,
    private changeDetectorRef: ChangeDetectorRef,
    private fb: FormBuilder,
    private translate: TranslateService,
    private route: ActivatedRoute
  ) {
    this.gridOptions.getData = (params?: any) => {
      return this.kubeService.getList(params);
    };

    translate.get('kubernetes.grid')
      .subscribe(grid => this.handleGrid(grid));

    translate.get('kubernetes.addKubeForm.inputs.credentialType.options')
      .subscribe(options => this.handleCredentialTypes(options));
  }

  ngOnInit() {
    this.route.fragment.subscribe(fragment => {
      switch (fragment) {
        case 'add':
          this.openAddKube();
          break;

        default:
          // code...
          break;
      }
    });
  }

  selectedRowChange(rows: Array<Kubernetes>): void {
    this.selectedRows = rows;
  }

  openAddKube(): void {
    this.openModal('add');
  }

  addKube(): void {
    const formModel = this.addKubeForm.value;

    const kube: Kubernetes = {
      name: formModel.name,
      apiServerUrl: formModel.apiServerUrl,
      credential: {
        type: formModel.credentialType,
        value: undefined
      }
    };

    switch (formModel.credentialType) {
      case 'basic_auth':
        kube.credential.value = {
          username: formModel.username,
          password: formModel.password
        };
        break;

      case 'certificate':
        kube.credential.value = {
          certificate: formModel.certificate,
          private_key: formModel.key
        };
        break;

      case 'config':
        kube.credential.value = {
          file: formModel.configFile
        };
        break;

      default:
        console.warn('Whoops! a credential type wasn\'t caught');
        break;
    }

    this.kubeService.create(kube)
      .subscribe(response => {
        if (!response) {
          response = kube;
        }
        this.handleAdd(response);
      });
  }

  confirmDeleteKube(): void {
    this.openModal('delete');
  }

  deleteKube(): void {
    this.selectedRows.forEach(kube => {
      this.kubeService.delete(kube.id)
        .subscribe(response => this.handleDeletion(response));
    });
  }

  onConfigFileChange(event) {
    if (event.target.files.length === 0) {
      this.addKubeForm.patchValue({
        configFile: null
      });
      return;
    }

    const reader = new FileReader();
    reader.onload = () => {
      this.addKubeForm.patchValue({
        configFile: reader.result
      });
      this.changeDetectorRef.markForCheck();
    };
    reader.readAsText(event.target.files[0]);
  }

  private createAddKubeForm() {
    this.addKubeForm = this.fb.group({
      name: ['', Validators.required],
      apiServerUrl: ['', Validators.required],
      credentialType: ['basic_auth', Validators.required],
      username: '',
      password: '',
      certificate: '',
      key: '',
      configFile: '',
    });
  }

  private openModal(type: string): void {
    this.formType = type;

    switch (type) {
      case 'add':
        this.createAddKubeForm();
        this.modalSize = 'md';
        this.translate.get('kubernetes.addKubeForm.title')
          .subscribe(title => this.modalTitle = title);

        break;

      case 'delete':
        this.modalSize = 'sm';
        this.translate.get('kubernetes.deleteKubeForm.title')
          .subscribe(title => this.modalTitle = title);
        break;
    }

    this.openModalForm = true;
  }

  private handleAdd(response): void {
    console.log('add response', response);
    this.grid.addRow(response);
    this.openModalForm = false;
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.grid.reload();
    this.openModalForm = false;
  }

  private handleGrid(grid: any): void {
    this.gridOptions.paginationTitle = grid.pagination.title;
    this.gridOptions.columns = [{
      id: 'name',
      name: grid.columns.name.title,
      type: 'string'
    }, {
      id: 'apiServerUrl',
      name: grid.columns.apiServerUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return `http://${row.apiServerUrl}`;
      }
    }, {
      id: 'kubectlCliUrl',
      name: grid.columns.kubectlCliUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.kubectlCliUrl;
      }
    }, {
      id: 'helmCliUrl',
      name: grid.columns.helmCliUrl.title,
      type: 'externalLink',
      genLink: (row: Kubernetes) => {
        return row.helmCliUrl;
      }
    }, {
      id: 'ca',
      name: grid.columns.ca.title,
      type: 'string'
    }, {
      id: 'credentialType',
      name: grid.columns.credentialType.title,
      type: 'string'
    }, {
      id: 'insecure',
      name: grid.columns.insecure.title,
      type: 'string'
    }, {
      id: 'createdOn',
      name: grid.columns.created.title,
      type: 'date'
    }, {
      id: 'updatedOn',
      name: grid.columns.updated.title,
      type: 'date'
    }];
  }

  private handleCredentialTypes(options: any): void {
    this.credentialOptions.forEach(option => {
      option.name = options[option.value].title;
    });
  }
}
