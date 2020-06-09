/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { VmwComboboxItem } from './combobox-item.model';
import { Component, Input, OnChanges, EventEmitter, Output } from '@angular/core';


@Component({
  selector: 'concord-combobox-items',
  templateUrl: './combobox-items.component.html',
  styleUrls: ['./combobox-items.component.scss']
})
export class VmwComboboxItemsComponent implements OnChanges {
  @Input() items: Array<VmwComboboxItem>;
  @Input() noItemsFoundString: string;
  @Input() position: string;
  @Input() multiSelect = false;
  @Input() filterIncludes = false;
  @Input() showLoading = false;

  /**
   * Emits an array of items when multiSelect === true. Otherwise emits a ComboboxItem.
   */
  @Output() selectionChange = new EventEmitter<VmwComboboxItem|Array<VmwComboboxItem>>();


  public filteredItems: Array<VmwComboboxItem>;
  private selectedIndex = -1;
  /**
   * Contains a set of selected items when multiSelect === true.
   */
  private selectedItems: Set<VmwComboboxItem> = new Set();

  selectNext() {
    if (this.filteredItems == null || this.filteredItems.length === 0) {
      return;
    }
    if (this.selectedIndex === this.filteredItems.length - 1) {
      this.selectedIndex = 0;
    } else {
      this.selectedIndex++;
    }
    this.selectionChange.emit(this.filteredItems[this.selectedIndex]);
  }

  selectPrev() {
    if (this.filteredItems == null || this.filteredItems.length === 0) {
      return;
    }
    if (this.selectedIndex === 0 || this.selectedIndex === -1) {
      this.selectedIndex = this.filteredItems.length - 1;
    } else {
      this.selectedIndex--;
    }
    this.selectionChange.emit(this.filteredItems[this.selectedIndex]);
  }

  hasItemWithPrefix(prefix: string): boolean {
    if (this.filteredItems == null || this.filteredItems.length === 0) {
      return false;
    }
    return this.filteredItems.some(
      this.displayValueStartsWith.bind(this, prefix));
  }

  private displayValueStartsWith(prefix: string, item: VmwComboboxItem): boolean {
    if (!prefix || item == null) {
      return false;
    }
    return item.displayValue.toLowerCase().startsWith(prefix.toLowerCase());
  }

  private displayValueWith(prefix: string, item: VmwComboboxItem): boolean {
    if (!prefix || item == null) {
      return false;
    }
    return item.displayValue.toLowerCase().includes(prefix.toLowerCase());
  }

  getFirstItemContainingPrefix(prefix: string): VmwComboboxItem {
    if (this.filteredItems == null || this.filteredItems.length === 0) {
      return null;
    }
    return this.filteredItems.find(
      this.displayValueStartsWith.bind(this, prefix));
  }

  filter(prefix: string) {
    if (this.items == null || this.items.length === 0) {
      return;
    }
    if (!prefix) {
      this.filteredItems = this.items;
    } else {
      this.filteredItems = this.items.filter(this.filterIncludes ?
        this.displayValueWith.bind(this, prefix) :
        this.displayValueStartsWith.bind(this, prefix)
      );
    }
  }

  findByDisplayValue(value: string): VmwComboboxItem {
    if (this.filteredItems == null || this.filteredItems.length === 0 || !value) {
      return null;
    }
    return this.filteredItems.find((item) => {
      return item.displayValue.toLowerCase() === value.toLowerCase();
    });
  }

  onItemClick(item: VmwComboboxItem) {
    if (this.multiSelect) {
      if (this.selectedItems.has(item)) {
        this.selectedItems.delete(item);
      } else {
        this.selectedItems.add(item);
      }
      this.selectionChange.emit(Array.from(this.selectedItems));
    } else {
      this.selectionChange.emit(item);
      this.selectItem(item);
    }
  }

  selectItem(itemToSelect: VmwComboboxItem) {
    this.selectedIndex = this.items.findIndex((item) => {
      return itemToSelect === item;
    });
  }

  selectItems(itemsToSelect: Array<VmwComboboxItem>) {
    this.selectedItems.clear();
    itemsToSelect.forEach((item) => {
      this.selectedItems.add(item);
    });
    this.selectionChange.emit(Array.from(this.selectedItems));
  }

  clearSelection() {
    this.selectedIndex = -1;
  }

  ngOnChanges(changes: any) {
    if (changes && changes.items) {
      this.filteredItems = changes.items.currentValue;
      if (changes.items.firstChange || !changes.items.previousValue) {
        this.selectedIndex = -1;
      }
    }
  }
}
