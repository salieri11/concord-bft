/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
  ViewChild
} from '@angular/core';

import { environment } from '../../../environments/environment';
import { OrgService } from '../shared/org.service';
import { Org } from '../shared/org.model';
import { InivteUserComponent } from '../inivte-user/inivte-user.component';
import { ContextualHelpService } from './../../shared/contextual-help.service';

@Component({
  selector: 'concord-org-list',
  templateUrl: './org-list.component.html',
  styleUrls: ['./org-list.component.scss']
})
export class OrgListComponent implements OnInit {
  @ViewChild('inviteUser', { static: true }) inviteUser: InivteUserComponent;
  orgs: Org[] = [];
  env = environment;

  constructor(
    private orgService: OrgService,
    private helpService: ContextualHelpService
  ) {
    this.orgService.getList().subscribe(orgs => this.orgs = orgs);
  }

  ngOnInit() { }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }
}
