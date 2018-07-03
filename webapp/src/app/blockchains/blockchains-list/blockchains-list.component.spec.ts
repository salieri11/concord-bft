/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';

import { BlockchainsService } from '../shared/blockchains.service';
import { BlockchainsListComponent } from './blockchains-list.component';
import { GridModule } from '../../grid/grid.module';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { HttpClient } from '@angular/common/http';
import { HttpLoaderFactory } from '../../app.module';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MockSharedModule } from '../../shared/shared.module';

describe('BlockchainsListComponent', () => {
  let component: BlockchainsListComponent;
  let fixture: ComponentFixture<BlockchainsListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        GridModule,
        HttpClientTestingModule,
        TranslateModule.forRoot({
          loader: {
            provide: TranslateLoader,
            useFactory: HttpLoaderFactory,
            deps: [HttpClient]
          }
        })
      ],
      declarations: [BlockchainsListComponent],
      providers: [
        BlockchainsService,

      ]
    })
      .overrideModule(GridModule, {
        set: {
          imports: [
            FormsModule,
            MockSharedModule,
            RouterModule
          ],
        }
      })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockchainsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
