/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  Output,
  EventEmitter,
  Input
} from '@angular/core';
import {
  style,
  animate,
  transition,
  trigger,
  state
} from '@angular/animations';

/*
    To use AccordionComponent, include:
        title property,
        tag with .accordion-header class (will be projected into accordion header),
        tag with .accordion-body class (will be projected into accordion body),
 */

@Component({
  selector: 'concord-accordion',
  templateUrl: 'accordion.component.html',
  styleUrls: ['./accordion.component.scss'],
  animations: [
    trigger(
      'collapse', [
        state('in', style({
          'height': '*',
          'overflow': 'hidden'
        })),

        transition(':enter', [
          style({
            'height': '0',
            'overflow': 'hidden'
          }),
          animate('0.2s ease-in-out'),
        ]),

        transition(':leave', [
          style({
            'height': '*',
            'overflow': 'hidden'
          }),
          animate('0.2s ease-in-out', style({height: 0}))
        ]),
      ]
    )
  ]
})

export class VmwAccordionComponent {
  @Input() title: string = null;
  @Input() isExpanded = false;
  @Input() expandable = true;
  @Input() isOverlayed = false;
  @Output() expandChange = new EventEmitter();
  @Output() toggleSupportEmitter = new EventEmitter();

  toggleExpand() {
    if (!this.expandable) {
      return;
    }
    this.isExpanded = !this.isExpanded;
    this.expandChange.emit(this.isExpanded);
  }
}
