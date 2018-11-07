/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';

import { HighlightService } from '../../highlight.service';

@Component({
  selector: 'athena-code-highlighter',
  templateUrl: './code-highlighter.component.html',
  styleUrls: ['./code-highlighter.component.scss']
})
export class CodeHighlighterComponent implements OnChanges {
  @Input() language: string;
  @Input() breakWord: boolean = false;
  @Input() noScroll: boolean = false;
  @Input() noBorder: boolean = false;
  @Input() textToHighlight: string;

  classList: string;
  highlightedText: string;
  constructor(private highlighter: HighlightService) { }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.textToHighlight.currentValue) {
      const highlightLanguage = this.language === 'c-like' ? 'solidity' : this.language;
      this.classList = `language-${this.language} ${this.breakWordClass()} ${this.noScrollClass()} ${this.noBorderClass()}`;
      this.highlightedText = this.highlighter.highlight(this.textToHighlight, this.highlighter.languages[highlightLanguage]);
    }
  }

  private breakWordClass() {
    return this.breakWord ? 'break-word' : '';
  }

  private noBorderClass() {
    return this.noBorder ? 'no-border' : '';
  }

  private noScrollClass() {
    return this.noScroll ? 'no-scroll' : '';
  }

}
