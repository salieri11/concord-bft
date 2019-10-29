/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit } from '@angular/core';
import * as _ from 'lodash';

@Component({
  selector: 'concord-log-details',
  templateUrl: './log-details.component.html',
  styleUrls: ['./log-details.component.scss']
})
export class LogDetailsComponent implements OnInit {
  @Input() log: any;

  message: any;
  objectKeys = Object.keys;

  constructor() {
  }

  ngOnInit() {
    this.message = this.convertStringToObj(this.log.message);
  }

  // parse message  string from API
  convertStringToObj(str: string) {
    const jsonMessage = str
      .replace(/=/g, '\":\"')
      .replace(/{/g, '{\"')
      .replace(/"{/g, '{')
      .replace(/,\s/g, ', \"')
      .replace(/,/g, '\",')
      .replace(/}/g, '\"}')
      .replace(/}"/g, '}')
      .replace(/"\[/g, '[\"')
      .replace(/\]"/g, '\"]');

    return JSON.parse(jsonMessage);
  }

  isResObject(res): boolean {
    return typeof res === 'object' ? true : false;
  }

  getFormattedKey(key: string): string {
    return _.startCase(key);
  }

}
