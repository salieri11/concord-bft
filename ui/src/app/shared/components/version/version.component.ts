/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { VersionService, Version } from '../../version.service';

@Component({
  selector: 'concord-version',
  templateUrl: './version.component.html',
  styleUrls: ['./version.component.scss']
})
export class VersionComponent {
  ver: Version = {version: 'noResponse', commit: 'noResponse'};

  constructor(
    private versionService: VersionService
  ) {
    this.versionService.getCurrent().subscribe((version: Version) => {
      this.ver = version;
    });
  }
}
