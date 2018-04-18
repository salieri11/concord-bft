/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Injectable, NgModule, Pipe, PipeTransform } from '@angular/core';

@Pipe({name: 'translate'})
export class MockTranslatePipe implements PipeTransform {
  transform(value: string): string {
    return value;
  }
}

@Injectable()
export class TranslateService {
  getBrowserLang() {}
  setDefaultLang() {}
  use() {}
}

@NgModule({
  providers: [TranslateService],
  declarations: [MockTranslatePipe],
  exports: [MockTranslatePipe]
})
export class MockTranslateModule { }

