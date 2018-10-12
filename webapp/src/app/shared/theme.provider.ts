/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { Subject } from 'rxjs';

import { VmwCookieUtil } from './cookie-util';

export enum VmwClarityTheme {
  Dark = 'Dark',
  Light = 'Light',
}

const THEME_COOKIE_NAME = 'clarity-theme';
const THEME_COOKIE_DOMAIN = '/';
const CLARITY_CSS_LINK_ID = 'clarity-css';
const CLARITY_CSS_DARK_PATH = '/assets/static/css/clr-ui-dark.min.css';
const CLARITY_CSS_LIGHT_PATH = '/assets/static/css/clr-ui.min.css';

export class VmwClarityThemeConfig {
  constructor(
    public clarityDarkPath = CLARITY_CSS_DARK_PATH,
    public clarityLightPath = CLARITY_CSS_LIGHT_PATH,
    public cookieName = THEME_COOKIE_NAME,
    public darkBodyClasses = ['fade-to-dark', 'dark'],
    public cookieDomain = THEME_COOKIE_DOMAIN) {}
}

@Injectable()
export class VmwClarityThemeService {
  public themeChange = new Subject<string>();
  private config = new VmwClarityThemeConfig();

  constructor(@Inject(DOCUMENT) private document: any) {}

  initialize(providedConfig?: VmwClarityThemeConfig) {
    return new Promise((resolve) => {
      if (providedConfig) {
        this.config = Object.assign(this.config, providedConfig);
      }

      const theme = VmwCookieUtil.getCookie(this.config.cookieName);
      const clarityCssLink = this.document.createElement('link');

      clarityCssLink.rel = 'stylesheet';
      clarityCssLink.id = CLARITY_CSS_LINK_ID;

      if (theme === VmwClarityTheme.Dark) {
        clarityCssLink.href = this.config.clarityDarkPath;

        let bodyClasses = 'dark';

        if (this.config.darkBodyClasses) {
          bodyClasses = this.config.darkBodyClasses.join(' ');
        }

        this.document.body.className += ' ' + bodyClasses;

      } else {
        clarityCssLink.href = this.config.clarityLightPath;
      }

      this.document.head.appendChild(clarityCssLink);

      if (theme === VmwClarityTheme.Dark) {
        setTimeout(() => {
          resolve(theme);
        }, 2500);
      } else {
        resolve(theme);
      }
    });
  }

  public get theme() {
    let currentTheme = VmwCookieUtil.getCookie(this.config.cookieName);

    if (!currentTheme) {
      currentTheme = VmwClarityTheme.Light;
    }

    return currentTheme;
  }

  public set theme(newTheme: string) {
    const clarityCssLink = this.document.getElementById(CLARITY_CSS_LINK_ID) as HTMLLinkElement;

    if (newTheme === VmwClarityTheme.Dark) {
      let bodyClasses = 'dark';

      if (this.config.darkBodyClasses) {
        bodyClasses = this.config.darkBodyClasses.join(' ');
      }

      this.document.body.className = bodyClasses;

      clarityCssLink.href = this.config.clarityDarkPath;

    } else {
      this.document.body.className = '';

      clarityCssLink.href = this.config.clarityLightPath;
    }

    VmwCookieUtil.setCookie(THEME_COOKIE_NAME, newTheme,
      '/',
      VmwCookieUtil.TEN_YEARS,
      this.config.cookieDomain);

    this.themeChange.next(newTheme);
  }
}
