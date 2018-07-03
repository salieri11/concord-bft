/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { Kubernetes } from './shared/kubernetes.model';
import { Personas } from '../shared/persona.service';
import { KubernetesFormComponent } from './kubernetes-form/kubernetes-form.component';
import { KubernetesListComponent } from './kubernetes-list/kubernetes-list.component';


@Component({
  selector: 'athena-kubernetes',
  templateUrl: './kubernetes.component.html',
  styleUrls: ['./kubernetes.component.scss']
})
export class KubernetesComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('kubernetesForm') kubernetesForm: KubernetesFormComponent;
  @ViewChild('kubernetesList') kubernetesList: KubernetesListComponent;

  selected: Array<Kubernetes>;

  constructor() {
  }

  ngOnInit() {
  }

  kubernetesSelectionChange(rows: Array<Kubernetes>): void {
    this.selected = rows;
  }

  addKubernetes(kube: Kubernetes) {
    this.kubernetesList.grid.addRow(kube);
  }

  deleteKubernetes() {
    this.kubernetesList.grid.reload();
  }

}
