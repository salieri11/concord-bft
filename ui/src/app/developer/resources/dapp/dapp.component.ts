/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit
} from '@angular/core';
import { DappService } from './dapp.service';


@Component({
  selector: 'concord-dapp',
  templateUrl: './dapp.component.html',
  styleUrls: ['./dapp.component.scss']
})
export class DappComponent implements OnInit {
  deploying: boolean;
  alreadyDeployed: boolean;

  constructor(private deployService: DappService) { }

  ngOnInit() {
    // We will add this back in once we
    // this.checkIfDeployed();
  }

  deploy() {
    this.deploying = true;
    this.deployService.deploySupplyChainDapp()
      .subscribe(
        () => this.handleSuccessfulDeploy(),
        () => this.handleError()
        );
  }

  checkIfDeployed() {
    this.deployService.checkIfDeployed()
      .subscribe(
        response => this.alreadyDeployed = response.data.buildExists,
        () => this.handleError()
      );
  }

  private handleSuccessfulDeploy(): void {
    this.deploying = false;
  }

  private handleError(): void {
    this.deploying = false;
  }

}
