/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MockSharedModule } from '../../shared.module';
import { VersionComponent } from './version.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { VersionService } from '../../version.service';

describe('VersionComponent', () => {
  let component: VersionComponent;
  let fixture: ComponentFixture<VersionComponent>;
  let element: Element;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ MockSharedModule, HttpClientTestingModule ],
      declarations: [ VersionComponent ],
      providers: [ VersionService ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(VersionComponent);
    element = fixture.debugElement.nativeElement;
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should have help text', () => {
    const helpText = element.querySelector('p');
    expect(helpText.innerText).toContain('common.help blockchain-support@vmware.com');
  });

});
