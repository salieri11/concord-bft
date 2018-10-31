/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { HighlightService } from '../../shared/highlight.service';

@Component({
  selector: 'athena-connect-with-truffle',
  templateUrl: './connect-with-truffle.component.html',
  styleUrls: ['./connect-with-truffle.component.scss']
})
export class ConnectWithTruffleComponent implements OnInit {
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

  constructor(private highlighter: HighlightService) { }

  ngOnInit() {
    this.highlightCode();
  }

  openModal() {
    this.open = true;
  }
  private highlightCode() {
    this.truffleSetup = this.highlighter.highlight(
      this.truffleSetup, this.highlighter.languages.json
    );
    this.truffleExample1 = this.highlighter.highlight(
      this.truffleExample1, this.highlighter.languages.javascript
    );
    this.truffleExample2 = this.highlighter.highlight(
      this.truffleExample2, this.highlighter.languages.javascript
    );
    this.truffleDeployString = this.highlighter.highlight(
      this.truffleDeployString, this.highlighter.languages.json
    );
  }


}
