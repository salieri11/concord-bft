/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { RouterTestingModule } from '@angular/router/testing';
import { testFor, prepareEach, beforeTesting } from '../../../test.helper.spec';

import { NodeListComponent } from './node-list.component';
import { DeployClientComponent } from './../deploy-client/deploy-client.component';
import { NodesStatusFilterComponent } from '../nodes-status-filter/nodes-status-filter.component';
import { VmwCopyToClipboardButtonComponent } from '../../shared/components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { CanViewDirective } from '../../shared/directives/can-view.directive';
import { ClarityModule } from '@clr/angular';
import { HttpClientTestingModule } from '@angular/common/http/testing';


describe('NodeListComponent', () => {
  const test = testFor(NodeListComponent).expedite({
    imports: [
      ClarityModule,
      RouterTestingModule,
      HttpClientTestingModule,
    ],
    declarations: [
      NodeListComponent,
      NodesStatusFilterComponent,
      VmwCopyToClipboardButtonComponent,
      DeployClientComponent,
      CanViewDirective
    ],
  }, beforeTesting(() => { }), prepareEach(() => { }));

  it('should create', () => {
    expect(test.component).toBeTruthy();
  });
});
