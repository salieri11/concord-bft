/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { VmwComboboxItem } from './combobox-items/combobox-item.model';
import { VmwComboboxItemsComponent } from './combobox-items/combobox-items.component';
import { Component, ElementRef, forwardRef, Input, ViewChild, HostListener, ChangeDetectorRef, OnInit } from '@angular/core';
import { ControlValueAccessor, NG_VALUE_ACCESSOR } from '@angular/forms';
import { KeyCodes } from './key-codes';

export const TIMEOUT = 200;

@Component({
  selector: 'concord-combobox',
  templateUrl: './combobox.component.html',
  styleUrls: ['./combobox.component.scss'],
  providers: [{
    provide: NG_VALUE_ACCESSOR,
    useExisting: forwardRef(() => VmwComboboxComponent),    // tslint:disable-line:no-forward-ref
    multi: true
  }]
})
export class VmwComboboxComponent implements ControlValueAccessor, OnInit {
  @Input() items: Array<VmwComboboxItem>;
  @Input() placeholder = '';
  @Input() showSuggestionsOnFocus = true;
  @Input() onFocusMinInputLength = 0;
  @Input() isAddNewAllowed = true;
  @Input() filterItemsWhenTyping = false;
  @Input() autoComplete = false;
  @Input() isValid = true;
  @Input() errorMessage = '';
  @Input() noItemsFoundString: string;
  @Input() multiSelect = false;
  @Input() filterIncludes = false;
  @Input() position = 'bottom-left';
  @Input() showTooltip = true;
  @Input() showLoading = false;

  public showSuggestions = false;

  @ViewChild(VmwComboboxItemsComponent, { static: false })
  private comboboxItems: VmwComboboxItemsComponent;
  @ViewChild('inputControl', { static: false })
  private input: ElementRef;
  // ControlValueAccessor interface implementation.
  private onChangeCallback: (_: any) => {};
  private onTouchedCallback: () => {};

  constructor(private elementRef: ElementRef, protected changeDetectorRef: ChangeDetectorRef) { }

  private _displayValue = '';
  get displayValue(): string {
    return this._displayValue;
  }

  set displayValue(value: string) {
    if (this.filterItemsWhenTyping) {
      if (this.comboboxItems) { this.comboboxItems.filter(value); }
    }
    if (!this.isAddNewAllowed && this.comboboxItems &&
      !this.comboboxItems.findByDisplayValue(value) &&
      value) {
      return;
    }
    if (value !== this._displayValue) {
      this._displayValue = value;
      let comboboxItem = !this.comboboxItems ? null
                        : this.comboboxItems.findByDisplayValue(value);
      if (!comboboxItem) {
        // Create a fake item if isAddNewAllowed is true and the
        // matching item is not found.
        comboboxItem = new VmwComboboxItem(value, value);
      }

      try { this.changeDetectorRef.detectChanges(); } catch (e) {}

      if (this.onChangeCallback) {
        this.onChangeCallback(comboboxItem);
      }
    }
  }

  onKeyPress(event: KeyboardEvent) {
    const prefix = (this.displayValue ? this.displayValue : '') + event.key;
    if (!this.isAddNewAllowed && !this.comboboxItems.findByDisplayValue(prefix)) {
      event.preventDefault();
      event.stopImmediatePropagation();
    }
  }

  onKeyUp(event: KeyboardEvent) {
    const input =  this.input.nativeElement;
    let autocompleteItem;

    if (this.autoComplete) {
      autocompleteItem = this.comboboxItems.getFirstItemContainingPrefix(this.displayValue);
    }
    switch (event.keyCode) {
      case KeyCodes.Delete:
      case KeyCodes.Backspace:
        return;
      case KeyCodes.Up:
        this.comboboxItems.selectPrev();
        break;
      case KeyCodes.Down:
        this.comboboxItems.selectNext();
        break;
      case KeyCodes.Enter:
        if (autocompleteItem) {
          this.displayValue = autocompleteItem.displayValue;
          this.comboboxItems.selectItem(autocompleteItem);
          input.blur();
        } else if (this.isAddNewAllowed) {
          this.comboboxItems.clearSelection();
          input.blur();
        }
        break;
      default:
        if (autocompleteItem) {
          const start = this.displayValue.length;
          const end = autocompleteItem.displayValue.length;
          input.value = autocompleteItem.displayValue;
          input.setSelectionRange(start, end);
        }
    }
  }

  onFocus() {
    if (this.showSuggestionsOnFocus
        && this.input.nativeElement.value.length >= this.onFocusMinInputLength) {
      this.showSuggestions = true;
    }
    if (this.multiSelect) {
      this.input.nativeElement.blur();
      return;
    }
    this.input.nativeElement.setSelectionRange(0, this.input.nativeElement.value.length);
  }

  onBlur() {
    if (this.multiSelect) {
      return;
    }
    // Timeout the hide to catch a click event on the list when the user
    // clicks the comobobox-items.
    setTimeout(() => {
      this.showSuggestions = false;
      if (this.onTouchedCallback) {
        this.onTouchedCallback();
      }
      try { this.changeDetectorRef.markForCheck(); } catch (e) {}
    }, TIMEOUT);
  }

  setDisplayValue(selection: VmwComboboxItem|Array<VmwComboboxItem>) {
    if (Array.isArray(selection)) {
      this.displayValue = selection.map((item: VmwComboboxItem) => {
        return item.displayValue;
      }).join(', ');
      if (this.onChangeCallback) { this.onChangeCallback(selection); }
    } else {
      this.displayValue = selection.displayValue;
    }
  }

  ngOnInit() {
    if (!this.placeholder) {
      this.placeholder = '';
    }
  }

  toggleComboboxItems() {
    this.showSuggestions = !this.showSuggestions;
    if (this.showSuggestions) {
      this.input.nativeElement.focus();
    }
    this.input.nativeElement.setSelectionRange(
      0, this.input.nativeElement.value.length);
  }

  // ControlValueAccessor interface implementation.
  writeValue(value: VmwComboboxItem | Array<VmwComboboxItem>) {
    if (!value) {
      this._displayValue = '';
      if (this.comboboxItems) {
        this.comboboxItems.clearSelection();
      }
      return;
    }
    if (Array.isArray(value)) {
      this.setDisplayValue(value);
      if (this.comboboxItems) { this.comboboxItems.selectItems(value); }
    } else if (value instanceof VmwComboboxItem || value['displayValue']) {
      if (!this.isAddNewAllowed && this.comboboxItems &&
        !this.comboboxItems.findByDisplayValue(value.displayValue)) {
        throw new Error(
          'The item with the given displayValue does not exist: ' +
          value.displayValue);
      }
      if (value.displayValue !== this._displayValue) {
        this._displayValue = value.displayValue;
        if (this.comboboxItems) {
          this.comboboxItems.selectItem(value);
        }
      }
    } else {
      throw new Error('The type should be an instance of ComboboxItem');
    }
  }

  /**
   * Set the function to be called when the control receives a change event.
   * This function is called by Angular when the NgModel binding is used.
   */
  registerOnChange(fn: any) {
    this.onChangeCallback = fn;
  }

  /**
   * Set the function to be called when the control receives a touch event.
   * This function is called by Angular when the NgModel binding is used.
   */
  registerOnTouched(fn: any) {
    this.onTouchedCallback = fn;
  }

  @HostListener('document:click', ['$event'])
  documentClick(event: Event) {
    // Closes the dropd-down if the user clicked outside of the component.
    if (!this.elementRef.nativeElement.contains(event.target)) {
      this.showSuggestions = false;
      this.onTouchedCallback();
    }
  }
}
