/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { NgModule, ModuleWithProviders } from '@angular/core';

import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ClarityModule, ClrFormsNextModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { MockTranslateModule } from '../mocks/mock-translate.module';

import { AuthenticationService } from './authentication.service';
import { AuthenticatedGuard } from './authenticated-guard.service';
import { AgreementGuard } from './agreement-guard.service';
import { ANDES_API_PREFIX, ATHENA_API_PREFIX, ETHEREUM_API_PREFIX } from './shared.config';
import { TransactionsStatusFilterComponent } from './components/transactions-status-filter/transactions-status-filter.component';
import { RouterModule } from '@angular/router';
import { PersonaService } from './persona.service';
import { CanViewDirective } from './directives/can-view.directive';
import { VmwTaskComponent } from './components/task-panel/task.component';
import { VmwTaskPanelComponent } from './components/task-panel/task-panel.component';
import { VmwTasksService } from './components/task-panel/tasks.service';
import { BlockchainWizardComponent } from './components/blockchain-wizard/blockchain-wizard.component';
import { VmwAccordionComponent } from './components/accordion/accordion.component';
import { VmwAccordionGroupComponent } from './components/accordion/accordion-group.component';
import { VmwComboboxComponent } from './components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from './components/combobox/combobox-items/combobox-items.component';
import { VmwCopyToClipboardButtonComponent } from './components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { VmwThemeSwitchButtonComponent } from './components/theme-switch-button/theme-switch-button.component';
import { CodeHighlighterComponent } from './components/code-highlighter/code-highlighter.component';
import { AppHeaderComponent } from './components/app-header/app-header.component';

@NgModule({
  imports: [
    CommonModule,
    TranslateModule,
    ClarityModule,
    ClrFormsNextModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule
  ],
  declarations: [
    TransactionsStatusFilterComponent,
    CanViewDirective,
    VmwTaskComponent,
    VmwTaskPanelComponent,
    VmwAccordionComponent,
    VmwAccordionGroupComponent,
    VmwComboboxComponent,
    VmwComboboxItemsComponent,
    VmwCopyToClipboardButtonComponent,
    BlockchainWizardComponent,
    VmwThemeSwitchButtonComponent,
    CodeHighlighterComponent,
    AppHeaderComponent
  ],
  exports: [
    CommonModule,
    TranslateModule,
    ClarityModule,
    ClrFormsNextModule,
    TransactionsStatusFilterComponent,
    ReactiveFormsModule,
    CanViewDirective,
    VmwTaskComponent,
    VmwTaskPanelComponent,
    VmwAccordionComponent,
    VmwAccordionGroupComponent,
    VmwComboboxComponent,
    VmwComboboxItemsComponent,
    VmwCopyToClipboardButtonComponent,
    BlockchainWizardComponent,
    VmwThemeSwitchButtonComponent,
    CodeHighlighterComponent,
    AppHeaderComponent
  ]
})
export class SharedModule {
  public static forRoot(): ModuleWithProviders {
    return {
      ngModule: SharedModule,
      providers: [
        AuthenticationService,
        AuthenticatedGuard,
        AgreementGuard,
        {provide: ANDES_API_PREFIX, useValue: 'api'},
        {provide: ATHENA_API_PREFIX, useValue: 'api/athena'},
        {provide: ETHEREUM_API_PREFIX, useValue: 'api/athena/eth'},
        PersonaService,
        VmwTasksService
      ]
    };
  }
}

@NgModule({
  imports: [
    CommonModule,
    MockTranslateModule,
    ClarityModule,
    ClrFormsNextModule,
    ReactiveFormsModule
  ],
  providers: [
    AuthenticationService,
    AuthenticatedGuard,
    {provide: ANDES_API_PREFIX, useValue: 'api'},
    {provide: ATHENA_API_PREFIX, useValue: 'api/athena'},
    {provide: ETHEREUM_API_PREFIX, useValue: 'api/athena/eth'},
    PersonaService
  ],
  exports: [
    CommonModule,
    MockTranslateModule,
    ClarityModule,
    ReactiveFormsModule
  ]
})
export class MockSharedModule {}

