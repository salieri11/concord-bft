/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ChangeDetectorRef, Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TranslateService } from '@ngx-translate/core';

import { CredType, Kubernetes } from '../shared/kubernetes.model';
import { KubernetesService } from '../shared/kubernetes.service';
import { Personas } from '../../shared/persona.service';

@Component({
  selector: 'athena-kubernetes-form',
  templateUrl: './kubernetes-form.component.html',
  styleUrls: ['./kubernetes-form.component.scss']
})
export class KubernetesFormComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @Input('selected') selected: Array<Kubernetes>;
  @Output() addKubernetes: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteKubernetes: EventEmitter<any> = new EventEmitter<any>();

  openModalForm = false;
  modalTitle = '';
  formType: string;
  modalSize = 'md';
  addKubeForm: FormGroup;
  deleteKubeForm: FormGroup;

  get credentialType(): any {
    return this.addKubeForm.get('credentialType');
  }

  credType = CredType;
  credentialOptions: Array<{ name?: string; value: string }> = [
    { value: CredType.BasicAuth, name: 'kubernetes.addKubeForm.inputs.credentialType.options.basic_auth.title' },
    { value: CredType.Certificate, name: 'kubernetes.addKubeForm.inputs.credentialType.options.certificate.title' },
    { value: CredType.ConfigFile, name: 'kubernetes.addKubeForm.inputs.credentialType.options.configFile.title' },
  ];

  constructor(
    private kubeService: KubernetesService,
    private changeDetectorRef: ChangeDetectorRef,
    private fb: FormBuilder,
    private translate: TranslateService,
    private route: ActivatedRoute
  ) {

    this.handleGrid();
    this.translate.get('kubernetes.addKubeForm.inputs.credentialType.options')
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
    this.selected.forEach(kube => {
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
    this.addKubernetes.emit(response);
    this.openModalForm = false;
  }

  private handleDeletion(response): void {
    console.log('deletion response', response);
    this.deleteKubernetes.emit();
    this.openModalForm = false;
  }

  private handleCredentialTypes(options: any): void {
    this.credentialOptions.forEach(option => {
      option.name = options[option.value].title;
    });
  }

}
