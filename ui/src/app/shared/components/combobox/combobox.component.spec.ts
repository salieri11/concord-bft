/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { TestBed, fakeAsync, tick } from '@angular/core/testing';
import { VmwComboboxComponent, TIMEOUT } from './combobox.component';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { VmwComboboxItem } from './combobox-items/combobox-item.model';
import { VmwComboboxItemsComponent } from './combobox-items/combobox-items.component';
import { BrowserModule } from '@angular/platform-browser';
import { ViewChild } from '@angular/core';
import { ClarityModule } from '@clr/angular';
import { KeyCodes } from './key-codes';


const keyUp = (control: any, keyCode: number = -1) => {
  const event = new CustomEvent('keyup', {
    bubbles: true,
    cancelable: true
  });
  Object.defineProperty(event, 'keyCode', {
    get : function() {
      return keyCode;
    }
  });
  control.dispatchEvent(event);
};

const keyPress = (control: any, key: string): CustomEvent => {
  const event = new CustomEvent('keypress', {
    bubbles: true,
    cancelable: true
  });
  Object.defineProperty(event, 'key', {
    get : function() {
      return key;
    }
  });
  spyOn(event, 'preventDefault').and.callThrough();
  spyOn(event, 'stopImmediatePropagation').and.callThrough();
  control.dispatchEvent(event);

  return event;
};

const select = (control: any, item: VmwComboboxItem) => {
  const elements = Array.prototype.slice.call(control.querySelectorAll('.dropdown-item'));
  const itemToClick = elements.find((element: HTMLElement) => {
    return element.innerText.indexOf(item.displayValue) !== -1;
  });

  itemToClick.click();
};


// A component to trigger the ngOnChanges method of the sddc-status component.
// See https://goo.gl/6zv5So for more details.
@Component({
  template: `
      <concord-combobox name="combobox"
          [items]="items"
          [placeholder]="placeholder"
          [showSuggestionsOnFocus]="showSuggestionsOnFocus"
          [isAddNewAllowed]="isAddNewAllowed"
          [(ngModel)]="modelValue"
          [filterItemsWhenTyping]="filterItemsWhenTyping"
          [autoComplete]="autoComplete"
          [multiSelect]="multiSelect">
      </concord-combobox>`
})
class TestHostComponent {
  @ViewChild(VmwComboboxComponent, /* TODO: add static flag */ {})
  // @ts-ignore: no unused locals
  private comboboxComponent: VmwComboboxComponent;
  modelValue = '';
  onChange() {
    return;
  }
}


describe('ComboboxComponent', () => {
  let fixture: any;
  let component: any;
  let element: any;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        FormsModule,
        BrowserModule,
        ClarityModule
      ],
      declarations: [
        VmwComboboxComponent,
        TestHostComponent,
        VmwComboboxItemsComponent
      ]
    });

    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    element = fixture.nativeElement;
    fixture.detectChanges();
    spyOn(component, 'onChange');
  });

  describe('when the items are not set', () => {
    it('hides the control', () => {
      expect(element.querySelector('concord-combobox').children.length).toBe(0);
    });
  });

  describe('when the items are set to an empty array', () => {
    it('hides the control', () => {
      expect(element.querySelector('concord-combobox').children.length).toBe(0);
    });
  });

  describe('when the items are set', () => {
    let input: HTMLInputElement;
    let item1: VmwComboboxItem;
    let item2: VmwComboboxItem;

    beforeEach(() => {
      item1 = new VmwComboboxItem('first item', 'value1');
      item2 = new VmwComboboxItem('second item', 'value2');
      component.items = [item1, item2];
      fixture.detectChanges();
      input = element.querySelector('input');
    });

    it('shows the control', () => {
      expect(element.querySelector('concord-combobox').children.length)
        .toBeGreaterThan(0);
    });

    describe('writeValue', () => {
      describe('when called with an object that does not exist in items', () => {
        it('throws an error', () => {
          expect(() => {
            component.comboboxComponent.writeValue(new VmwComboboxItem('test', 'test'));
          }).toThrow();
        });
      });
    });

    describe('and the placeholder is not set', () => {
      it('does not show a placeholder', () => {
        expect(input.placeholder).toEqual('');
      });
    });

    describe('and the placeholder is set', () => {
      beforeEach(() => {
        component.placeholder = 'test placeholder';
        fixture.detectChanges();
      });

      it('shows a placeholder', () => {
        expect(input.placeholder).toEqual('test placeholder');
      });
    });

    describe('and the showSuggestionsOnFocus is true', () => {
      beforeEach(() => {
        component.showSuggestionsOnFocus = true;
        fixture.detectChanges();
      });

      it('keeps the combobox-items hidden before focus event occurs', () => {
        expect(element.querySelector('concord-combobox-items').hidden).toBe(true);
      });

      describe('and the user focuses the input', () => {
        beforeEach(() => {
          input.focus();
          fixture.detectChanges();
        });

        it('shows the combobox-items', () => {
          expect(element.querySelector('concord-combobox-items'))
            .not.toBe(null);
        });

        describe('and the mutiSelect is true', () => {
          beforeEach(() => {
            component.multiSelect = true;
            fixture.detectChanges();
          });

          describe('and the user selects multiple items', () => {
            beforeEach(() => {
              select(element, item1);
              select(element, item2);
            });

            it('sets the model to an array of selected items', () => {
              expect(component.modelValue).toEqual([item1, item2]);
            });
          });
        });

        describe('navigation', () => {
          describe('when the down key is pressed', () => {
            beforeEach(() => {
              keyUp(input, KeyCodes.Down);
              fixture.detectChanges();
            });

            // test disabled due to false positive
            it('selects the first item', () => {
              expect(element.querySelector('.active span').innerText)
                .toContain('first item');
            });
          });

          describe('and the up key is pressed', () => {
            beforeEach(() => {
              keyUp(input, KeyCodes.Up);
              fixture.detectChanges();
            });

            // test disabled due to false positive
            it('selects the last item', () => {
              expect(element.querySelector('.active span').innerText)
                .toContain('second item');
            });
          });
        });

        describe('and the user enters a value', () => {
          beforeEach(() => {
            input.value = 'first item';
            input.dispatchEvent(new Event('input'));
          });

          it('keeps the suggestion box opened', () => {
            expect(element.querySelector('concord-combobox-items'))
              .not.toBe(null);
          });

          describe('and the blur event ocurs', () => {
            it('closes the suggestion box', fakeAsync(() => {
              input.value = 'test value';
              input.dispatchEvent(new Event('blur'));
              tick(TIMEOUT);
              fixture.detectChanges();

              expect(element.querySelector('concord-combobox-items').hidden)
                .toBe(true);
            }));
          });
        });

        describe('and the filterItemsWhenTyping is true', () => {
          beforeEach(() => {
            component.filterItemsWhenTyping = true;
            fixture.detectChanges();
          });

          describe('and the user enters a value (enteredValue)', () => {
            beforeEach(() => {
              input.value = 'first';
              input.dispatchEvent(new Event('input'));
              fixture.detectChanges();
            });

            it('shows only items that satisfy the condition startsWith(enteredValue)', () => {
              expect(element.querySelectorAll('.dropdown-item').length).toBe(1);
              expect(element.querySelector('.dropdown-item').innerText).toContain('first item');
            });
          });
        });

        describe('and the filterItemsWhenTyping is false', () => {
          beforeEach(() => {
            component.filterItemsWhenTyping = false;
            fixture.detectChanges();
          });

          describe('and the user enters a value (enteredValue)', () => {
            beforeEach(() => {
              input.value = 'first';
              input.dispatchEvent(new Event('input'));
              fixture.detectChanges();
            });

            it('shows all available items', () => {
              expect(element.querySelectorAll('.dropdown-item').length).toBe(2);
            });
          });
        });
      });
    });

    describe('and the showSuggestionsOnFocus is false', () => {
      beforeEach(() => {
        component.showSuggestionsOnFocus = false;
        fixture.detectChanges();
      });

      describe('and the user focuses the input', () => {
        beforeEach(() => {
          input.focus();
        });

        it('keeps the suggestions hidden', () => {
          expect(element.querySelector('concord-combobox-items').hidden).toBe(true);
        });
      });
    });

    describe('and the isAddNewAllowed is true', () => {
      beforeEach(() => {
        component.isAddNewAllowed = true;
        fixture.detectChanges();
      });

      describe('and the user types a new value that does not exist in the items', () => {
        let event: any;

        beforeEach(() => {
          event = keyPress(input, 'v');
          fixture.detectChanges();
        });

        it('continue propagating the event', () => {
          expect(event.stopImmediatePropagation)
            .not.toHaveBeenCalled();
        });

        it('does not call preventDefault', () => {
          expect(event.preventDefault).not.toHaveBeenCalled();
        });
      });

      describe('and the user types a new value that does not exist in the items', () => {
        beforeEach(() => {
          input.value = 'value that does not exist';
          input.dispatchEvent(new Event('input'));
          fixture.detectChanges();
        });

        it('sets the model', () => {
          expect(component.modelValue.displayValue).toEqual('value that does not exist');
          expect(component.modelValue.value).toEqual('value that does not exist');
        });

        describe('and the enter key is pressed', () => {
          beforeEach(() => {
            spyOn(input, 'blur')
              .and.callThrough();
            component.comboboxComponent.comboboxItems.selectedIndex = 0;
            keyUp(input, KeyCodes.Enter);
          });

          it('clears the selection', () => {
            expect(component.comboboxComponent.comboboxItems.selectedIndex)
              .toBe(-1);
          });

          it('triggers the input"s blur function', () => {
            expect(input.blur).toHaveBeenCalled();
          });
        });
      });
    });

    describe('and the isAddNewAllowed is false', () => {
      beforeEach(() => {
        component.isAddNewAllowed = false;
        fixture.detectChanges();
      });

      describe('and the user types a new value that does not exist in the items', () => {
        beforeEach(() => {
          keyPress(input, 'v');
          fixture.detectChanges();
        });

        it('keeps the model unchanged', () => {
          expect(component.modelValue).toBe('');
        });

        it('keeps the input unchanged', () => {
          expect(input.value).toBe('');
        });
      });

      describe('and the autoComplete is true', () => {
        beforeEach(() => {
          component.autoComplete = true;
          fixture.detectChanges();
        });

        describe('and the user enters a value that is a prefix of an existing item', () => {
          beforeEach(() => {
            component.isAddNewAllowed = true;
            fixture.detectChanges();
            component.comboboxComponent.displayValue = 'first';
            fixture.detectChanges();
            keyUp(input);
          });

          it('appends the rest of the item"s value', () => {
            expect(input.value).toBe('first item');
          });

          describe('and the Enter key is pressed', () => {
            beforeEach(() => {
              keyUp(input, KeyCodes.Enter);
              fixture.detectChanges();
            });

            it('selects the matching item', () => {
              expect(component.modelValue).toBe(item1);
            });
          });
        });
      });
    });

    describe('setDisplayValue', () => {
      describe('when called', () => {
        beforeEach(() => {
          component.comboboxComponent.setDisplayValue(item1);
          fixture.detectChanges();
        });

        it('sets the displayValue', () => {
          expect(component.comboboxComponent.displayValue)
            .toBe('first item');
        });

        it('clears the displayValue if the clear button was clicked', () => {
          const clearElement = element.querySelector('.clear-icon');
          clearElement.click();
          expect(component.comboboxComponent.displayValue).toBe('');
        });
      });
    });

    describe('writeValue', () => {
      describe('when called with an object that is not instance of VmwComboboxItem', () => {
        it('throws an error', () => {
          expect(() => {
            component.comboboxComponent.writeValue({});
          }).toThrow();
        });
      });
    });

    describe('toggleComboboxItems', () => {
      it('sets the showSuggestions variable to false by default', () => {
        expect(component.comboboxComponent.showSuggestions)
          .toBe(false);
      });

      describe('when called', () => {
        beforeEach(() => {
          component.comboboxComponent.toggleComboboxItems();
          fixture.detectChanges();
        });

        it('sets the showSuggestions variable to true', () => {
          expect(component.comboboxComponent.showSuggestions)
            .toBe(true);
        });
      });
    });
  });
});
