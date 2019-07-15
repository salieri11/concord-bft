/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, NgZone } from '@angular/core';

import { VmwClarityThemeService, VmwClarityTheme } from '../../theme.provider';

@Component({
    selector: 'concord-theme-switch-button',
    templateUrl: 'theme-switch-button.component.html',
    styleUrls: ['./theme-switch-button.component.scss'],
})
export class VmwThemeSwitchButtonComponent {
    public VmwClarityTheme = VmwClarityTheme;
    theme: string;

    constructor(public themeService: VmwClarityThemeService) {
      this.theme = themeService.theme;
      this.themeService.themeChange
        .subscribe(theme => this.theme = theme);
    }

    themeChanged(newTheme: string) {
         this.themeService.theme = newTheme;
    }


}
