/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { OrgListComponent } from './org-list/org-list.component';
import { Personas } from '../shared/persona.service';
import { Org } from './shared/org.model';
import { OrgFormComponent } from './org-form/org-form.component';


@Component({
  selector: 'concord-org',
  templateUrl: './orgs.component.html',
  styleUrls: ['./orgs.component.scss']
})
export class OrgsComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('orgList') orgList: OrgListComponent;
  @ViewChild('orgForm') orgForm: OrgFormComponent;

  selected: Array<Org>;

  constructor() {
  }

  ngOnInit() {
  }

  orgsSelectionChange(rows: Array<Org>): void {
    this.selected = rows;
  }

  addOrg(org: Org) {
    this.orgList.grid.addRow(org);
  }

  deleteOrg() {
    this.orgList.grid.reload();
  }
}
