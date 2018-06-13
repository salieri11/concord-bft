import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { GridComponent } from './grid.component';
import { ClarityModule } from "@clr/angular";
import { FormsModule }   from '@angular/forms';
import { BrowserModule }    from '@angular/platform-browser';
import { TranslateModule } from '@ngx-translate/core';

import { RouterModule } from '@angular/router';
import { ErrorAlertService } from '../shared/global-error-handler.service'


@NgModule({
  imports: [
    CommonModule,
    ClarityModule,
    FormsModule,
    BrowserModule,
    TranslateModule,
    RouterModule
  ],
  declarations: [GridComponent],
  providers: [ErrorAlertService],
  exports: [
      GridComponent,
  ],
})
export class GridModule { }
