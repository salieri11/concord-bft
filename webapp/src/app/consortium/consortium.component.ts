/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { Personas } from '../shared/persona.service';
import { ConsortiumFormComponent } from './consortium-form/consortium-form.component';
import { ConsortiumsListComponent } from './consortiums-list/consortiums-list.component';
import { Consortium } from './shared/consortium.model';

@Component({
  selector: 'athena-consortium',
  templateUrl: './consortium.component.html',
  styleUrls: ['./consortium.component.scss']
})
export class ConsortiumComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('consortiumForm') consortiumForm: ConsortiumFormComponent;
  @ViewChild('consortiumsList') consortiumsList: ConsortiumsListComponent;

  selected: Array<Consortium>;

  constructor() {}

  ngOnInit() {}

  selectedRowChange(rows: Array<Consortium>): void {
    this.selected = rows;
  }

  addToConsortiumsList(response) {
    this.consortiumsList.grid.addRow(response);
  }

  deleteFromConsortiumsList() {
    this.consortiumsList.grid.reload();
  }
}
