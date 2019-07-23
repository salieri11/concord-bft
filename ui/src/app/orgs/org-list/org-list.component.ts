/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
} from '@angular/core';

import { environment } from '../../../environments/environment';
import { OrgService } from '../shared/org.service';
import { Org } from '../shared/org.model';

@Component({
  selector: 'concord-org-list',
  templateUrl: './org-list.component.html',
  styleUrls: ['./org-list.component.scss']
})
export class OrgListComponent implements OnInit {
  orgs: Org[] = [];
  env = environment;

  constructor(
    private orgService: OrgService,
  ) {
    this.orgService.getList().subscribe(orgs => this.orgs = orgs);
  }

  ngOnInit() {}
}
