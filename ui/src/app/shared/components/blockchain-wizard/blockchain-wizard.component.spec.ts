/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ClrFormsNextModule } from '@clr/angular';
import { BlockchainWizardComponent } from './blockchain-wizard.component';
import { MockSharedModule } from '../../shared.module';
import { VmwComboboxComponent } from '../combobox/combobox.component';
import { VmwAccordionGroupComponent } from '../accordion/accordion-group.component';
import { VmwAccordionComponent } from '../accordion/accordion.component';
import { VmwComboboxItemsComponent } from '../combobox/combobox-items/combobox-items.component';

describe('BlockchainWizardComponent', () => {
  let component: BlockchainWizardComponent;
  let fixture: ComponentFixture<BlockchainWizardComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        ClrFormsNextModule,
        RouterTestingModule,
        MockSharedModule,
        FormsModule
      ],
      declarations: [
        BlockchainWizardComponent,
        VmwComboboxComponent,
        VmwComboboxItemsComponent,
        VmwAccordionGroupComponent,
        VmwAccordionComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainWizardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('User page', () => {
    it('appends a user to the main form on user create', () => {
      expect(component.form.get('users').value.length).toEqual(0);

      component.userForm.setValue({
        email: 'test@example.com',
        role: 'system_admin'
      });

      component.addUser();
      expect(component.form.get('users').value.length).toEqual(1);
    });

    it('resets the user form after user creation', () => {
      const filledFormState = {
        email: 'test@example.com',
        role: 'system_admin'
      };
      spyOn(component.userForm, 'reset').and.callThrough();

      expect(component.userForm.value.email).toBeFalsy();

      component.userForm.setValue(filledFormState);
      expect(component.userForm.value.email).toBe(filledFormState.email);

      component.addUser();
      expect(component.userForm.reset).toHaveBeenCalled();
      expect(component.userForm.value.email).toBeFalsy();
    });

    it('deletes a user from the main form based on its index', () => {
      const users = [{
        email: 'test1@example.com',
        role: 'system_admin'
      }, {
        email: 'test2@example.com',
        role: 'system_admin'
      }];

      expect(component.form.get('users').value.length).toBe(0);

      users.forEach((user) => {
        component.userForm.setValue(user);
        component.addUser();
      });

      expect(component.form.get('users').value.length).toBe(2);
      expect(component.form.get('users').value[1].email).toBe(users[1].email);

      component.deleteUser(1);

      expect(component.form.get('users').value[1]).toBeUndefined();
      expect(component.form.get('users').value.length).toBe(1);
    });
  });

  describe('On open', () => {
    it('resets all forms', () => {
      spyOn(component.form, 'reset');
      spyOn(component.userForm, 'reset');

      component.open();

      expect(component.form.reset).toHaveBeenCalled();
      expect(component.userForm.reset).toHaveBeenCalled();
    });

    it('resets the clarity wizard', () => {
      spyOn(component.wizard, 'reset');

      component.open();

      expect(component.wizard.reset).toHaveBeenCalled();
    });

    it('sets isOpen', () => {
      expect(component.isOpen).toBe(false);

      component.open();

      expect(component.isOpen).toBe(true);
    });
  });

  describe('On submit', () => {
    it('emits the form value', () => {
      spyOn((component as any).router, 'navigate');
      spyOn(component.setupComplete, 'emit');

      component.onSubmit();

      expect(component.setupComplete.emit).toHaveBeenCalledWith(component.form.value);
    });

    it('navigates to the dashboard', () => {
      const routerSpy = spyOn((component as any).router, 'navigate');
      spyOn(component, 'resetFragment').and.callThrough();

      component.onSubmit();

      expect(component.resetFragment).toHaveBeenCalled();
      expect(routerSpy).toHaveBeenCalledWith(['/dashboard']);
    });
  });
});
