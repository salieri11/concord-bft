/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { VmwComboboxItem } from './combobox-item.model';
import { VmwComboboxItemsComponent } from './combobox-items.component';
import { Component } from '@angular/core';
import { TestBed } from '@angular/core/testing';
import { ViewChild } from '@angular/core';
import { ClarityModule } from '@clr/angular';

// A component to trigger the ngOnChanges method of the sddc-status component.
// See https://goo.gl/6zv5So for more details.
@Component({
  template: `
        <concord-combobox-items
              [items]="items"
              (selectionChange)="selectionChange($event)"
              [noItemsFoundString]="noItemsFoundString"
              [multiSelect]="multiSelect"
              [position]="position">
        </concord-combobox-items>`
})
class TestHostComponent {
  @ViewChild(VmwComboboxItemsComponent)
  // @ts-ignore: no unused locals
  private comboboxItemsComponent: VmwComboboxItemsComponent;
  position = 'bottom-left';
  selectionChange() {
    return;
  }
}

describe('VmwComboboxItems', () => {
  let fixture: any;
  let component: any;
  let element: any;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ClarityModule],
      declarations: [TestHostComponent, VmwComboboxItemsComponent]
    });
    fixture = TestBed.createComponent(TestHostComponent);
    component = fixture.componentInstance;
    element = fixture.nativeElement;
    fixture.detectChanges();
    spyOn(component, 'selectionChange');
  });

  describe('when the items variable is set', () => {
    let item1: VmwComboboxItem;
    let item2: VmwComboboxItem;

    beforeEach(() => {
      item1 = new VmwComboboxItem('test item 1', 'value1', 'user');
      item2 = new VmwComboboxItem('test item 2', 'value2');
      component.items = [item1, item2];
      component.position = 'bottom-right';
      fixture.detectChanges();
    });

    it('shows the available items', () => {
      expect(element.querySelectorAll('.dropdown-item').length).toEqual(2);
    });

    it('shows a clarity icons', () => {
      expect(element.querySelectorAll('clr-icon').length).toEqual(1);
    });

    it('should align the dropdown to a respective direction', () => {
      expect(element.querySelectorAll('.bottom-right').length).toEqual(1);
    });

    // selectNext and selectPrev.
    describe('and the selectNext is called', () => {
      beforeEach(() => {
        component.comboboxItemsComponent.selectNext();
        fixture.detectChanges();
      });

      it('selects the first item', () => {
        expect(element.querySelector('.active').innerText).toContain(item1.displayValue);
      });

      it('triggers the selectionChanged event with the first item', () => {
        expect(component.selectionChange).toHaveBeenCalledWith(item1);
      });

      describe('and the selectNext is called again', () => {
        beforeEach(() => {
          component.selectionChange.calls.reset();
          component.comboboxItemsComponent.selectNext();
          fixture.detectChanges();
        });

        it('selects the second item', () => {
          expect(element.querySelector('.active').innerText.trim()).toBe(item2.displayValue);
        });

        it('triggers the selectionChanged event with the second item',
          () => {
            expect(component.selectionChange).toHaveBeenCalledWith(item2);
          });

        describe('and the selectPrev is called', () => {
          beforeEach(() => {
            component.selectionChange.calls.reset();
            component.comboboxItemsComponent.selectPrev();
            fixture.detectChanges();
          });

          it('selects the first item', () => {
            expect(element.querySelector('.active').innerText).toContain(item1.displayValue);
          });

          it('triggers the selectionChanged event with the first item', () => {
            expect(component.selectionChange).toHaveBeenCalledWith(item1);
          });
        });
      });
    });

    // edge cases
    describe('and the selectNext function is called (totalItemsCount + 1) times', () => {
      beforeEach(() => {
        component.selectionChange.calls.reset();
        component.comboboxItemsComponent.selectNext();
        component.comboboxItemsComponent.selectNext();
        component.comboboxItemsComponent.selectNext();
        fixture.detectChanges();
      });

      it('selects the first item', () => {
        expect(component.selectionChange).toHaveBeenCalledWith(item1);
      });
    });

    describe('and the selectPrev function is called (totalItemsCount + 1) times', () => {
      beforeEach(() => {
        component.selectionChange.calls.reset();
        component.comboboxItemsComponent.selectPrev();
        component.comboboxItemsComponent.selectPrev();
        component.comboboxItemsComponent.selectPrev();
        fixture.detectChanges();
      });

      it('selects the first item', () => {
        expect(component.selectionChange).toHaveBeenCalledWith(item1);
      });
    });

    // Contains
    describe('and the hasItemWithPrefix function is called with existing value', () => {
      it('returns true', () => {
        expect(component.comboboxItemsComponent.hasItemWithPrefix('test i'))
          .toBe(true);
      });
    });

    describe('and the contains function is called with a value that does not exist', () => {
      it('returns false', () => {
        expect(component.comboboxItemsComponent.hasItemWithPrefix('non-existent prefix'))
          .toBe(false);
      });
    });

    // Filter.
    describe('and the filter function is called with an existing prefix', () => {
      beforeEach(() => {
        component.comboboxItemsComponent.filter(item1.displayValue);
        fixture.detectChanges();
      });

      it('matching item is shown', () => {
        expect(element.querySelectorAll('.dropdown-item').length).toBe(1);
        expect(element.querySelector('.dropdown-item').innerText.trim()).toBe(item1.displayValue);
      });
    });

    describe('and the filter function is called with a prefix that does not exist', () => {
      beforeEach(() => {
        component.comboboxItemsComponent.filter('non-existent prefix');
        fixture.detectChanges();
      });

      it('shows No items found', () => {
        expect(element.querySelectorAll('.dropdown-item').length).toBe(1);
        expect(element.querySelector('.dropdown-item').innerText).toBe(' No items found. ');
      });
    });

    describe('and the includes filter is true', () => {
      beforeEach(() => {
        component.comboboxItemsComponent.filterIncludes = true;
        component.comboboxItemsComponent.filter('item');
        fixture.detectChanges();
      });

      it('should filter items that contains a prefix', () => {
        expect(element.querySelectorAll('.dropdown-item').length).toBe(2);
      });
    });

    describe('and clicking on the item', () => {
      beforeEach(() => {
        component.selectionChange.calls.reset();
        const options = element.querySelectorAll('.dropdown-item');
        options[0].click();
        fixture.detectChanges();
      });

      it('triggers the selectionChange event with the selected item', () => {
        expect(component.selectionChange).toHaveBeenCalledWith(item1);
      });
    });

    describe('findByDisplayValue', () => {
      describe('when called with existing value', () => {
        it('returns an item', () => {
          const result = component.comboboxItemsComponent.findByDisplayValue('test item 1');
          expect(result.displayValue).toBe('test item 1');
        });
      });

      describe('when called with existing value', () => {
        it('returns null', () => {
          const result = component.comboboxItemsComponent.findByDisplayValue('does not exist');
          expect(result).toBeUndefined();
        });
      });
    });

    describe('selectItem', () => {
      describe('when selectItem is called with the first item', () => {
        beforeEach(() => {
          component.comboboxItemsComponent.selectItem(item1);
        });

        it('sets the selectedIndex to 0', () => {
          expect(component.comboboxItemsComponent.selectedIndex).toBe(0);
        });

        describe('and clearSelection is called', () => {
          beforeEach(() => {
            component.comboboxItemsComponent.clearSelection();
          });

          it('sets the selectedIndex to -1', () => {
            expect(component.comboboxItemsComponent.selectedIndex).toBe(-1);
          });
        });
      });
    });

    describe('onItemClick', () => {
      describe('when onItemClick is called with the first item', () => {
        beforeEach(() => {
          component.comboboxItemsComponent.onItemClick(item1);
        });

        it('sets the selectedIndex to 0', () => {
          expect(component.comboboxItemsComponent.selectedIndex).toBe(0);
        });
      });
    });

    describe('and the multiSelect is true', () => {
      beforeEach(() => {
        component.multiSelect = true;
        fixture.detectChanges();
      });

      describe('and clicking on the first item', () => {
        beforeEach(() => {
          component.selectionChange.calls.reset();
          const options = element.querySelectorAll('.dropdown-item');
          options[0].click();
          fixture.detectChanges();
        });

        it('triggers the selectionChange event with the selected item', () => {
          expect(component.selectionChange).toHaveBeenCalledWith([item1]);
        });

        describe('and clicking on the second item', () => {
          beforeEach(() => {
            component.selectionChange.calls.reset();
            const options = element.querySelectorAll('.dropdown-item');
            options[1].click();
            fixture.detectChanges();
          });

          it('triggers the selectionChange event with the selected items', () => {
            expect(component.selectionChange).toHaveBeenCalledWith([item1, item2]);
          });
        });
      });
    });
  });

  describe('when the items variable is not set', () => {
    describe('and the noItemsFoudString is not set', () => {
      it('shows No items available', () => {
        expect(element.querySelectorAll('.dropdown-item').length).toEqual(1);
        expect(element.querySelector('.dropdown-item').innerText).toEqual(' No items found. ');
      });
    });

    describe('and the noItemsFoudString is set', () => {
      beforeEach(() => {
        component.noItemsFoundString = 'No Items found custom string';
        fixture.detectChanges();
      });

      it('shows the given value', () => {
        expect(element.querySelectorAll('.dropdown-item').length)
          .toEqual(1);
        expect(element.querySelector('.dropdown-item').innerText)
          .toEqual(' No Items found custom string ');
      });
    });

    describe('and the selectNext method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.selectNext.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });

    describe('and the selectPrev method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.selectPrev.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });

    describe('and the hasItemWithPrefix method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.hasItemWithPrefix.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });

    describe('and the getFirstItemContainingPrefix method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.getFirstItemContainingPrefix.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });

    describe('and the filter method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.filter.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });

    describe('and the findByDisplayValue method is called', () => {
      it('does not throw an exception', () => {
        expect(component.comboboxItemsComponent.findByDisplayValue.bind(
          component.comboboxItemsComponent))
          .not.toThrow();
      });
    });
  });
});

