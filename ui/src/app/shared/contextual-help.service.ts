/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class ContextualHelpService {
  public HELP_URL = {
    STAGING: 'https://docs-staging.vmware.com/en/VMware-Blockchain/index.html',
    PROD: 'https://docs.vmware.com/en/VMware-Blockchain/index.html',
  };
  public HELP_TOPIC_URL = {
    STAGING: 'https://docs-staging.vmware.com/en/VMware-Blockchain/1.0/context?id=',
    PROD: 'https://docs.vmware.com/en/VMware-Blockchain/1.0/context?id='
  };

  // TODO - change to session var later
  tempEnv = 'staging';

  getHelpLink() {
    const helpHomeUrlList = this.HELP_URL;
    const helpTopicUrlList = this.HELP_TOPIC_URL;

    let helpHomeURL = helpHomeUrlList.PROD;
    let helpTopicURL = helpTopicUrlList.PROD;

    let env = this.tempEnv;
    if (!env) {
      env = 'prod';
    }

    if (env.toUpperCase() === 'STAGING') {
      helpTopicURL = helpTopicUrlList.STAGING;
      helpHomeURL = helpHomeUrlList.STAGING;
    }
    return { helpTopicURL: helpTopicURL, helpHomeURL: helpHomeURL };
  }

  openInNewTab(url: string) {
    const win = window.open(url, '_blank');
    if (win) {
      win.focus();
    }
  }

  openHelpPage(helpId: string) {
    const helpLink = this.getHelpLink();
    let helpTopicURL = helpLink.helpTopicURL;
    if (helpId) {
      helpTopicURL = helpTopicURL + helpId;
    }
    this.openInNewTab(helpTopicURL);
  }

  openHelpHome() {
    const helpLink = this.getHelpLink();
    this.openInNewTab(helpLink.helpHomeURL);
  }

}
