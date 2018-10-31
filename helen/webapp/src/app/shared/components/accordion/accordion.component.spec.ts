/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { VmwAccordionComponent } from './accordion.component';
import { VmwAccordionGroupComponent } from './accordion-group.component';
import { TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ClarityModule } from '@clr/angular';

// TestWrapperClass allows us to test the content projection of NetworkSettingComponent
@Component({
  selector: 'athena-test-wrapper',
  template: `
        <athena-accordion
            [title]="'title thats always there'">
            <div class="accordion-subheader"><span>Our Accordion Subheader</span></div>
            <div class="accordion-body">Our Accordion Body</div>
        </athena-accordion>
    `
})
class TestWrapperClassComponent {
}

describe('Accordion', () => {
  let fixture: any;
  let el: any;

  beforeEach((done) => {
    TestBed.configureTestingModule({
      imports: [
        BrowserAnimationsModule,
        ClarityModule
      ],
      declarations: [
        VmwAccordionComponent,
        VmwAccordionGroupComponent,
        TestWrapperClassComponent,
      ],
    }).compileComponents().then(() => {
      fixture = TestBed.createComponent(TestWrapperClassComponent);
      el = fixture.debugElement.nativeElement;
      fixture.detectChanges();
      done();
    });
  });

  it('shows accordion header and hides accordion body', () => {
    const header = el.querySelector('athena-accordion .accordion-subheader');
    expect(header.textContent).toContain('Our Accordion Subheader');
    expect(el.querySelector('clr-icon').getAttribute('shape')).toContain('caret right');
  });

  it('hides accordion header and shows accordion body', () => {
    el.querySelector('athena-accordion .accordion-header').click();
    fixture.detectChanges();

    const body = el.querySelector('athena-accordion .accordion-body');
    expect(body.textContent).toContain('Our Accordion Body');
    expect(el.querySelector('clr-icon').getAttribute('shape')).toContain('caret down');
  });

  afterEach(() => {
    fixture.destroy();
  });
});
