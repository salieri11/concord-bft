/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NgModule, ModuleWithProviders } from '@angular/core';

import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { ClarityModule } from '@clr/angular';
import { TranslateModule } from '@ngx-translate/core';
import { VmwContextualHelpModule } from '@vmw/ngx-contextual-help';
import { CspComponentsModule } from '@vmw/csp-ngx-components';
import { VmwComponentsModule } from '@vmw/ngx-components';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

import { AuthenticationService } from './authentication.service';
import { AuthenticatedGuard } from './authenticated-guard.service';
import { AgreementGuard } from './agreement-guard.service';
import { CONCORD_API_PREFIX, CSP_API_PREFIX, ETHEREUM_API_PREFIX, LOG_API_PREFIX } from './shared.config';
import { TransactionsStatusFilterComponent } from './components/transactions-status-filter/transactions-status-filter.component';
import { RouterModule } from '@angular/router';
import { PersonaService } from './persona.service';
import { CanViewDirective } from './directives/can-view.directive';
import { FeatureFlagDirective } from './directives/feature-flag.directive';
import { VmwTaskComponent } from './components/task-panel/task.component';
import { VmwTaskPanelComponent } from './components/task-panel/task-panel.component';
import { VmwTasksService } from './components/task-panel/tasks.service';
import { VmwAccordionComponent } from './components/accordion/accordion.component';
import { VmwAccordionGroupComponent } from './components/accordion/accordion-group.component';
import { VmwComboboxComponent } from './components/combobox/combobox.component';
import { VmwComboboxItemsComponent } from './components/combobox/combobox-items/combobox-items.component';
import { VmwCopyToClipboardButtonComponent } from './components/copy-to-clipboard-button/copy-to-clipboard-button.component';
import { VmwThemeSwitchButtonComponent } from './components/theme-switch-button/theme-switch-button.component';
import { CodeHighlighterComponent } from './components/code-highlighter/code-highlighter.component';
import { AppHeaderComponent } from './components/app-header/app-header.component';
import { VersionComponent } from './components/version/version.component';
import { ConfirmModalComponent } from './components/confirm-modal/confirm-modal.component';
import { RouterTestingModule } from '@angular/router/testing';
import { MockTranslateModule } from '../mocks/mock-translate.module';
import { VmwClarityThemeService } from './../shared/theme.provider';

export const defaultProvided: any[] = [
  AuthenticationService,
  AuthenticatedGuard,
  AgreementGuard,
  { provide: CONCORD_API_PREFIX, useValue: 'api/concord' },
  { provide: ETHEREUM_API_PREFIX, useValue: 'api/concord/eth' },
  { provide: LOG_API_PREFIX, useValue: 'logging/api' },
  { provide: CSP_API_PREFIX, useValue: 'csp/api' },
  PersonaService,
  VmwTasksService,
  VmwClarityThemeService
];

@NgModule({
  imports: [
    CommonModule,
    TranslateModule,
    ClarityModule,
    FormsModule,
    ReactiveFormsModule,
    RouterModule,
    VmwContextualHelpModule.forRoot(),
    CspComponentsModule.forRoot(),
    VmwComponentsModule.forRoot(),
  ],
  declarations: [
    TransactionsStatusFilterComponent,
    CanViewDirective,
    FeatureFlagDirective,
    VmwTaskComponent,
    VmwTaskPanelComponent,
    VmwAccordionComponent,
    VmwAccordionGroupComponent,
    VmwComboboxComponent,
    VmwComboboxItemsComponent,
    VmwCopyToClipboardButtonComponent,
    VmwThemeSwitchButtonComponent,
    CodeHighlighterComponent,
    AppHeaderComponent,
    VersionComponent,
    ConfirmModalComponent
  ],
  exports: [
    CommonModule,
    TranslateModule,
    ClarityModule,
    TransactionsStatusFilterComponent,
    FormsModule,
    ReactiveFormsModule,
    CspComponentsModule,
    CanViewDirective,
    FeatureFlagDirective,
    VmwTaskComponent,
    VmwTaskPanelComponent,
    VmwAccordionComponent,
    VmwAccordionGroupComponent,
    VmwComboboxComponent,
    VmwComboboxItemsComponent,
    VmwCopyToClipboardButtonComponent,
    VmwThemeSwitchButtonComponent,
    CodeHighlighterComponent,
    AppHeaderComponent,
    VersionComponent,
    ConfirmModalComponent
  ]
})
export class SharedModule {
  constructor() {
    if (window['UNIT_TEST_ENV']) { SharedModule.forRoot = SharedModule.forChild = SharedModule.forTesting; }
  }
  public static forRoot(): ModuleWithProviders {
    return { ngModule: SharedModule, providers: defaultProvided };
  }
  public static forChild(): ModuleWithProviders {
    return { ngModule: SharedModule, providers: defaultProvided };
  }
  public static forTesting(): ModuleWithProviders {
    return { ngModule: SharedModule, providers: [] };
  }
}

@NgModule({
  imports: [
    CommonModule,
    MockTranslateModule,
    ClarityModule,
    ReactiveFormsModule,
    RouterTestingModule,
    BrowserAnimationsModule,
    VmwContextualHelpModule.forRoot(),
    CspComponentsModule.forRoot(),
    VmwComponentsModule.forRoot(),
  ],
  providers: defaultProvided,
  exports: [
    CommonModule,
    MockTranslateModule,
    ClarityModule,
    FormsModule,
    ReactiveFormsModule,
    VmwContextualHelpModule,
    CspComponentsModule,
    VmwComponentsModule,
  ],
})
export class MockSharedModule {}
