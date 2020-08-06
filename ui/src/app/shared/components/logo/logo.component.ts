/*
 * Copyright 2018-2020 VMware, all rights reserved.
 */

import { Component, OnInit, Input } from '@angular/core';


@Component({
  selector: 'concord-logo',
  templateUrl: './logo.component.svg',
  styleUrls: ['./logo.component.scss']
})
export class LogoComponent implements OnInit {
  @Input() width: string;
  @Input() height: string;

  constructor() {
    this.width = this.width ? this.width : '450px';
    this.height = this.height ? this.height : '300px';
  }

  ngOnInit() {
  }

}
