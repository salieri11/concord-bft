/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

@Component({
  selector: 'athena-connect-with-truffle',
  templateUrl: './connect-with-truffle.component.html',
  styleUrls: ['./connect-with-truffle.component.scss']
})
export class ConnectWithTruffleComponent {
  open: boolean = false;
  truffleSetup: string = `npm install web3@0.19.0 --save`;
  truffleExample1: string = `
  Web3 = require('web3');

  module.exports = {
    networks: {
      development: {
        network_id: "*",
        provider: () => {
          return new Web3.providers.HttpProvider("https://<your-hostname>/api/athena/eth/", 5000, '<username>', '<password>');
        },
      }
    }
  }
  `;
  truffleExample2: string = `
  Web3 = require('web3');

  module.exports = {
    networks: {
      development: {
        network_id: "*",
        provider: () => {
          return new Web3.providers.HttpProvider("https://<username>:<password>@<your-host-name>/api/athena/eth/");
        },
      }
    }
  }
  `;
  truffleDeployString: string = `truffle migrate ---network=development`;

  constructor() { }

  openModal() {
    this.open = true;
  }
}
