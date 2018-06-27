/*
 * Copyright 2018 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';

import { OrgListComponent } from './org-list/org-list.component';


@Component({
  selector: 'athena-org-management',
  templateUrl: './org-management.component.html',
  styleUrls: ['./org-management.component.scss']
})
export class OrgManagementComponent implements OnInit {
  @ViewChild('orgList') orgList: OrgListComponent;

  constructor() {}

  ngOnInit() {}
}
