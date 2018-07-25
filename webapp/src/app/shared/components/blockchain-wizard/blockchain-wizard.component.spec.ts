/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';

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

  describe('Org page', () => {
    it('appends an organization to the main form on org create', () => {
      expect(component.form.get('organizations').value.length).toEqual(0);

      component.orgForm.setValue({
        name: 'Test Org',
        location: 'sydney'
      });

      component.addOrg();
      expect(component.form.get('organizations').value.length).toEqual(1);
    });

    it('resets the org form after org creation', () => {
      const filledFormState = {name: 'Test Org', location: 'sydney'};
      spyOn(component.orgForm, 'reset').and.callThrough();

      expect(component.orgForm.value.name).toBeFalsy();

      component.orgForm.setValue(filledFormState);
      expect(component.orgForm.value.name).toBe(filledFormState.name);

      component.addOrg();
      expect(component.orgForm.reset).toHaveBeenCalled();
      expect(component.orgForm.value.name).toBeFalsy();
    });

    it('deletes an org from the main form based on its index', () => {
      const orgs = [{name: 'Test Org', location: 'sydney'}, {name: 'Test Org 2', location: 'vienna'}];

      expect(component.form.get('organizations').value.length).toBe(0);

      orgs.forEach((org) => {
        component.orgForm.setValue(org);
        component.addOrg();
      });

      expect(component.form.get('organizations').value.length).toBe(2);
      expect(component.form.get('organizations').value[1].name).toBe(orgs[1].name);

      component.deleteOrg(1);

      expect(component.form.get('organizations').value[1]).toBeUndefined();
      expect(component.form.get('organizations').value.length).toBe(1);
    });
  });

  describe('User page', () => {
    it('appends a user to the main form on user create', () => {
      expect(component.form.get('users').value.length).toEqual(0);

      component.userForm.setValue({
        firstName: 'Test',
        lastName: 'User',
        email: 'test@example.com',
        organization: 'org',
        role: 'systems_admin'
      });

      component.addUser();
      expect(component.form.get('users').value.length).toEqual(1);
    });

    it('resets the user form after user creation', () => {
      const filledFormState = {
        firstName: 'Test',
        lastName: 'User',
        email: 'test@example.com',
        organization: 'org',
        role: 'systems_admin'
      };
      spyOn(component.userForm, 'reset').and.callThrough();

      expect(component.userForm.value.firstName).toBeFalsy();

      component.userForm.setValue(filledFormState);
      expect(component.userForm.value.firstName).toBe(filledFormState.firstName);

      component.addUser();
      expect(component.userForm.reset).toHaveBeenCalled();
      expect(component.userForm.value.firstName).toBeFalsy();
    });

    it('deletes a user from the main form based on its index', () => {
      const users = [{
        firstName: 'Test1',
        lastName: 'User2',
        email: 'test1@example.com',
        organization: 'org',
        role: 'systems_admin'
      }, {
        firstName: 'Test2',
        lastName: 'User2',
        email: 'test2@example.com',
        organization: 'org',
        role: 'systems_admin'
      }];

      expect(component.form.get('users').value.length).toBe(0);

      users.forEach((user) => {
        component.userForm.setValue(user);
        component.addUser();
      });

      expect(component.form.get('users').value.length).toBe(2);
      expect(component.form.get('users').value[1].firstName).toBe(users[1].firstName);

      component.deleteUser(1);

      expect(component.form.get('users').value[1]).toBeUndefined();
      expect(component.form.get('users').value.length).toBe(1);
    });
  });

  describe('Review page', () => {
    it('programmatically changes the current wizard page', () => {
      expect(component.wizard.isFirst).toBe(true);

      component.jumpTo(component.orgsPage);

      expect(component.wizard.isFirst).toBe(false);
      expect(component.wizard.currentPage).toBe(component.orgsPage);
    });

    it('populates the advanced settings form group with all public nodes, the consortium name, and number of nodes', () => {
      component.form.patchValue({
        consortium: {
          name: 'Test Consortium'
        }
      });
      const expectedValue = {
        numberOfNodes: 36,
        networkName: 'Test Consortium Net',
        publicNodesRegions: [...component.publicNodeItems]
      };

      expect(component.form.get('advancedSettings').value.numberOfNodes).toBeFalsy();
      expect(component.form.get('advancedSettings').value.networkName).toBeFalsy();
      expect(component.form.get('advancedSettings').value.publicNodesRegions).toBeFalsy();

      component.populateAdvancedSettingsForm();

      expect(component.form.get('advancedSettings').value.numberOfNodes).toBe(expectedValue.numberOfNodes);
      expect(component.form.get('advancedSettings').value.networkName).toBe(expectedValue.networkName);
      expect(component.form.get('advancedSettings').value.publicNodesRegions.length).toBe(expectedValue.publicNodesRegions.length);
    });
  });

  describe('On open', () => {
    it('resets all forms', () => {
      spyOn(component.form, 'reset');
      spyOn(component.orgForm, 'reset');
      spyOn(component.userForm, 'reset');

      component.open();

      expect(component.form.reset).toHaveBeenCalled();
      expect(component.orgForm.reset).toHaveBeenCalled();
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
