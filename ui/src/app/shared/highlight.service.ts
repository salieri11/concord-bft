/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';

declare var Prism: any;

@Injectable({
  providedIn: 'root'
})
export class HighlightService {

  constructor() {}

  highlight(text: string, language: string, options?): string {
    if (!text) { return ''; }
    if (!options) { options = {}; }
    const preprocessedText = this.preprocess(text, options);
    const processedText = Prism.highlight(preprocessedText, language);
    const postprocessedText = this.postprocess(processedText, options);
    return postprocessedText;
  }

  preprocess(text: string, options?): string {
    if (options.autolinker) {
      options.links = [];
      text = text.replace(/\[([^\]]*)\]\(([^)]*)\)/g, (_, g1, g2) => {
        options.links.push({name: g1, url: g2});
        return '__HIGHLIGHT_AUTOLINKER__' + options.links.length;
      });
    }
    return text;
  }

  postprocess(text: string, options?): string {
    if (options.autolinker) {
      let i = 1;
      for (const matched of options.links) {
        text = text.replace('__HIGHLIGHT_AUTOLINKER__' + i,
          `<a href="${matched.url}" target="_blank">${matched.name}</a>`);
        ++i;
      }
    }
    return text;
  }

  get languages() {
    return Prism.languages;
  }
}
