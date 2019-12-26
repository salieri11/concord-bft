/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnChanges, SimpleChanges,
  ViewChild, OnInit, OnDestroy, ElementRef } from '@angular/core';

import { HighlightService } from '../../highlight.service';
import { Router } from '@angular/router';
import { RouteService } from '../../route.service';

@Component({
  selector: 'concord-code-highlighter',
  templateUrl: './code-highlighter.component.html',
  styleUrls: ['./code-highlighter.component.scss']
})
export class CodeHighlighterComponent implements OnChanges, OnInit, OnDestroy {

  @ViewChild('preElement', { static: true }) preElement: ElementRef;

  @Input() language: string;
  @Input() breakWord: boolean = false;
  @Input() noScroll: boolean = false;
  @Input() noBorder: boolean = false;
  @Input() noMargin: boolean = false;
  @Input() minHeight: string;
  @Input() maxHeight: string;
  @Input() textToHighlight: string = '';
  @Input() otherStyles: string = '';
  @Input() plugins: string = '';

  classList: string;
  highlightedText: string;
  observer: MutationObserver;
  private initialized: boolean = false;
  private preInitChanges: SimpleChanges[] = [];

  constructor(
    private highlighter: HighlightService,
    private routeService: RouteService,
    private router: Router
    ) { }

  initialize() {
    if (this.initialized) { return; }
    this.initialized = true;
    const config = { attributes: false, childList: true, subtree: true };
    this.observer = new MutationObserver((list) => { this.handleMutationEvent(list); });
    this.observer.observe(this.preElement.nativeElement, config);
    for (const changes of this.preInitChanges) { this.ngOnChanges(changes); }
    this.preInitChanges = null;
  }

  ngOnInit() {
    this.initialize();
  }

  ngOnDestroy() {
    if (this.observer) { this.observer.disconnect(); }
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.textToHighlight) {
      const highlightLanguage = this.language === 'c-like' ? 'solidity' : this.language;

      this.classList = `language-${this.language} ${this.breakWordClass()}`
                      + ` ${this.noScrollClass()} ${this.noBorderClass()} ${this.noMarginClass()}`
                      + ` ${this.otherStyles}`;

      const highlightedText = this.highlighter.highlight(
        this.textToHighlight, this.highlighter.languages[highlightLanguage], {
          autolinker: this.hasPlugin('autolinker'),
        }
      );
      this.highlightedText = highlightedText;
    }
    if (changes.minHeight) {
      this.preElement.nativeElement.style.minHeight = this.minHeight ? this.minHeight : null;
    }
    if (changes.maxHeight) {
      if (!this.maxHeight) { this.maxHeight = '15rem'; }
      this.preElement.nativeElement.style.maxHeight = this.maxHeight;
    }
  }

  private hasPlugin(pluginName: string) {
    return (this.plugins && this.plugins.split(' ').indexOf(pluginName) >= 0);
  }

  private breakWordClass() {
    return this.breakWord ? 'break-word' : '';
  }

  private noBorderClass() {
    return this.noBorder ? 'no-border' : '';
  }

  private noMarginClass() {
    return this.noBorder ? 'no-margin' : '';
  }

  private noScrollClass() {
    return this.noScroll ? 'no-scroll' : '';
  }

  private handleMutationEvent(list: MutationRecord[]) {
    if (this.hasPlugin('autolinker')) {
      this.forAllAddedNodes(list, (el) => {
        if (el.nodeName === 'A') {
          el.onclick = (e) => {
            const href = el.getAttribute('href');
            if (href.startsWith('http://') || href.startsWith('https://')) {
              // Do nothing, and have this click redirect the user to the external destination
            }  else if (RouteService.isLinkAction(href)) {
              // Link action must trigger linkActionEvent but not change route
              this.routeService.triggerLinkActionEvent(href);
              e.preventDefault();
            } else {
              // Cancel redirection and use Angular router to go to the internal route
              this.router.navigate([href]);
              e.preventDefault();
            }
          };
        }
      });
    }
  }

  private forAllAddedNodes(list: MutationRecord[], func: (el: HTMLElement) => void) {
    for (const mutation of list) {
      for (let i = 0; i < mutation.addedNodes.length; ++i) {
        const target = mutation.addedNodes[i] as HTMLElement;
        const allElements = this.getAllElements(target) as HTMLElement[];
        for (const el of allElements) { func(el as HTMLAnchorElement); }
      }
    }
  }

  private getAllElements(el, list: any[] = []) {
    if (!el) { return list; } list.push(el);
    if (!el.children || el.children.length === 0) { return list; }
    for (let i = 0; i < el.children.length; ++i) { this.getAllElements(el.children[i], list); }
    return list;
  }

}
