/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { OrgListComponent } from './org-list/org-list.component';
import { Personas } from '../shared/persona.service';


@Component({
  selector: 'athena-org',
  templateUrl: './orgs.component.html',
  styleUrls: ['./orgs.component.scss']
})
export class OrgsComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];
  @ViewChild('orgList') orgList: OrgListComponent;

  constructor() {}

  ngOnInit() {}
}
