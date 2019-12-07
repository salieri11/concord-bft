/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { environment } from './../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class ContextualHelpService {
  env = environment;

  getHelpLink() {
    return { helpTopicURL: this.env.helpTopicUrl, helpHomeURL: this.env.helpUrl };
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
