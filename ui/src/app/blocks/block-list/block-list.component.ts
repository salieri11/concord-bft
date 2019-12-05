/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, Input, ViewChild, ElementRef, OnDestroy } from '@angular/core';

import { BlockListing, BlockInfo } from '../shared/blocks.model';
import { BlocksService } from '../shared/blocks.service';
import { mainRoutes, hexHash256HexRegExp } from '../../shared/urls.model';
import { delay } from 'rxjs/operators';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { ContextualHelpService } from './../../shared/contextual-help.service';

/**
 * Displays a paginated listing of blocks
 */
@Component({
  selector: 'concord-block-list',
  templateUrl: './block-list.component.html',
  styleUrls: ['./block-list.component.scss']
})
export class BlockListComponent implements OnInit, OnDestroy {
  @ViewChild('searchInput', { static: true }) searchInput: ElementRef;
  @Input() initialFetchCount: number = 1000;

  blocks: BlockInfo[] = [];
  blocksLoaded: boolean = false;
  nextBlockUrl: string;

  searching: boolean = false;
  autoExpandResult: boolean = false;
  searchInputMessage: string = '';
  searchQuery: string = '';
  searchResultShown: boolean = false;

  debounceTime: number = 500; // ms
  debouncer: {};

  constructor(
    private blocksService: BlocksService,
    private blockchainService: BlockchainService,
    private helpService: ContextualHelpService
  ) {}

  ngOnInit() {
    this.loadInitialBlocks();
    this.registerSearchExpansion();
  }

  ngOnDestroy() {
    this.deregisterSearchExpansion();
  }

  loadInitialBlocks() {
    if (!this.initialFetchCount) { this.initialFetchCount = 1000; }
    this.blocksService.getBlocks(this.initialFetchCount).subscribe(response => {
      this.handleBlocksResponse(response);
      this.blocksLoaded = true;
      // TODO: no need to fetch if getBlocks response already contains timestamp and tx count...
      for (const blockInfo of response.blocks) {
        this.blocksService.getBlock(blockInfo.number).subscribe(blockData => {
          const at = this.blocks.findIndex(item => item.hash === blockData.hash);
          this.blocks[at].blockData = blockData;
        });
      }
    });
  }

  loadNextBlocks() {
    this.blocksService.getBlocksByUrl(this.nextBlockUrl)
      .subscribe(response => this.handleBlocksResponse(response));
  }

  getBlockPageRouterLink(blockNumber) {
    if (!blockNumber && blockNumber !== 0) { return []; }
    return ['/' + this.blockchainService.blockchainId, mainRoutes.blocks, blockNumber];
  }

  handleSearchClear() {
    if (!this.searchQuery) { return; }
    this.searchQuery = '';
    this.searchInputMessage = '';
    this.searchResultShown = false;
    this.autoExpandResult = false;
    this.blocksLoaded = false;
    this.blocks = [];
    this.loadInitialBlocks();
    this.searchInput.nativeElement.focus();
  }

  handleSearch() {
    const inputValue = this.searchInput.nativeElement.value;
    if (inputValue === '') {
      if (this.searchResultShown) { this.handleSearchClear(); }
      return;
    }
    if (inputValue === this.searchQuery) { return; } // no need to try the same query
    if (!this.testSearchInput(inputValue)) {
      this.searchInputMessage = 'blocks.searchInputNotValid';
      return;
    }
    this.searchQuery = inputValue;
    this.searching = true;
    this.autoExpandResult = false;
    this.searchInputMessage = '';
    this.blocks = [];
    this.blocksService.getBlock(inputValue).pipe(delay(333)).subscribe(blockData => {
      this.searching = false;
      this.autoExpandResult = true;
      this.searchResultShown = true;
      this.blocks = [{
        number: blockData.number,
        hash: blockData.hash,
        url: null,
        blockData: blockData,
      }];
    }, (e) => {
      // TODO: handle no block found by hash or number
      console.log(e);
      this.searching = false;
      this.searchResultShown = true;
      this.searchInputMessage = 'blocks.searchNotFound';
    });
  }

  private handleBlocksResponse(response: BlockListing) {
    this.blocks = response.blocks;
    // TODO: Abstract to class with hasNext(), next(), etc. Support pagination in URL directly. Need to support prev?
    this.nextBlockUrl = response.next !== this.nextBlockUrl ? this.nextBlockUrl = response.next : undefined;
  }

  private registerSearchExpansion() {
    const input = this.searchInput.nativeElement;
    input.onblur = input.onchange = input.onkeyup = input.onpaste = () => {
      if (input.value.length < 24) {
        input.classList.remove('search-input-longtext');
        input.classList.remove('search-input-mediumtext');
      } else if (input.value.length >= 24 && input.value.length < 48) {
        input.classList.remove('search-input-longtext');
        input.classList.add('search-input-mediumtext');
      } else {
        input.classList.remove('search-input-mediumtext');
        input.classList.add('search-input-longtext');
      }
      // Auto search after 2 seconds
      const debouncer = this.debouncer = {};
      setTimeout(() => {
        if (debouncer === this.debouncer) { this.handleSearch(); }
      }, this.debounceTime);
    };
    input.onkeydown = (e) => {
      if (e.keyCode && e.keyCode === 13) { // Enter, search right away
        this.debouncer = {};
        this.handleSearch();
      }
    };
  }

  private deregisterSearchExpansion() {
    const input = this.searchInput.nativeElement;
    input.onblur = input.onchange = input.onkeyup = input.onpaste = null;
  }

  private testSearchInput(input: string) {
    if (input === '') {
      return false;
    } else if (/^\d*$/.test(input)) { // valid positive integer?
      const parsed = parseInt(input, 10);
      if (isNaN(parsed) || parsed >= Number.MAX_SAFE_INTEGER) { return false; } // max, 2^53 - 1
    } else if (!hexHash256HexRegExp.test(input)) { // valid hash of 0x{hex_64_len}?
      return false;
    }
    return true;
  }

  timestampToISOString(t) {
    return new Date(t).toISOString();
  }

  onClickToHelp(helpId) {
    this.helpService.openHelpPage(helpId);
  }

}
