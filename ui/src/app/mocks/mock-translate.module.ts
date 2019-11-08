/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable, NgModule, Pipe, PipeTransform, EventEmitter } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { of } from 'rxjs';

let _currentLang: string = null;
export const mockLanguagePack = {
  enabled: false, langs: {},
  setCurrentLang: (lang) => { _currentLang = lang; }
};

@Pipe({name: 'translate'})
export class MockTranslatePipe implements PipeTransform {
  transform(value: string) {
    if (mockLanguagePack.enabled && mockLanguagePack.langs[_currentLang]) {
      const translated = mockLanguagePack.langs[_currentLang][value];
      return (translated !== undefined) ? translated : value;
    }
    return value;
  }
}

@Injectable()
export class MockTranslateService  {
  public onLangChange: EventEmitter<any> = new EventEmitter();
  public onTranslationChange: EventEmitter<any> = new EventEmitter();
  public onDefaultLangChange: EventEmitter<any> = new EventEmitter();

  setDefaultLang() {}

  getBrowserLang() {}

  use(lang: string) { _currentLang = lang; }

  get(key: string) {
    if (mockLanguagePack.enabled && mockLanguagePack.langs[_currentLang]) {
      const translated = mockLanguagePack.langs[_currentLang][key];
      return translated ? of(translated) : of(key);
    }
    return of(key);
  }

  set(key: string, value: string): void {
    if (mockLanguagePack.enabled && mockLanguagePack.langs[_currentLang]) {
      mockLanguagePack.langs[_currentLang][key] = value;
    }
  }

  instant(key: string): string {
    if (mockLanguagePack.enabled && mockLanguagePack.langs[_currentLang]) {
      const translated = mockLanguagePack.langs[_currentLang][key];
      return translated ? translated : key;
    }
    return key;
  }
}

@NgModule({
  providers: [{provide: TranslateService, useClass: MockTranslateService}],
  declarations: [MockTranslatePipe],
  exports: [MockTranslatePipe]
})
export class MockTranslateModule {}

