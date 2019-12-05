/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
} from '@angular/core';
import { ContextualHelpService } from './../shared/contextual-help.service';

@Component({
  selector: 'concord-org',
  templateUrl: './orgs.component.html',
  styleUrls: ['./orgs.component.scss']
})
export class OrgsComponent implements OnInit {

  constructor(private helpService: ContextualHelpService) { }

  ngOnInit() { }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }

}
