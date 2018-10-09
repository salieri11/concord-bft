/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';

import { VmwClarityThemeService, VmwClarityTheme } from '../../theme.provider';

@Component({
    selector: 'athena-theme-switch-button',
    templateUrl: 'theme-switch-button.component.html',
    styleUrls: ['./theme-switch-button.component.scss'],
})
export class VmwThemeSwitchButtonComponent {
    public VmwClarityTheme = VmwClarityTheme;

    constructor(public themeService: VmwClarityThemeService) {}

    themeChanged(newTheme: string) {
        this.themeService.theme = newTheme;
    }
}
