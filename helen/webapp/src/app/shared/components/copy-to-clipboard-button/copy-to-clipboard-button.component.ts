/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { ElementRef, HostBinding, Output, EventEmitter, Component, OnInit, Input } from '@angular/core';

const SHOW_CHECKBOX_TIMEOUT = 1500;

@Component({
    selector: 'athena-copy-to-clipboard-button',
    templateUrl: './copy-to-clipboard-button.component.html',
    styleUrls: ['./copy-to-clipboard-button.component.scss']
})
export class VmwCopyToClipboardButtonComponent implements OnInit {
    @HostBinding('class') @Input('classList') classList: string = '';

    @Input() value: string;
    @Input() size = 16;
    @Input() tooltip = '';
    @Input() btnLabel = '';  // if no label specified, show the normal copy icon
    @Input() btnClasses = ['btn-outline'];  // if no label specified, show the normal copy icon
    @Input() disabled: boolean = false;
    @Input() tooltipDirection = 'top-left';

    @Output() copyClick = new EventEmitter<null>();

    btnClassesToApply: string;
    animClasses = 'flip-horizontal-reverse';
    bounds: string;
    hasProjectedContent: boolean = false;

    constructor(private el: ElementRef) {}

    ngOnInit() {
        this.bounds = (this.size + 2) + 'px';

        this.hasProjectedContent = this.el.nativeElement.innerText.trim();

        this.calculateClassesToApply();
    }

    calculateClassesToApply() {
        let classes: Array<string> = [];

        if (this.btnLabel && !this.btnLabel.length) {
            classes.push('icon-btn');
        }

        if (this.btnLabel && this.btnLabel.length) {
            classes = classes.concat(this.btnClasses);
        }

        if (this.disabled) {
            classes.push('disabled');
        }

        this.btnClassesToApply = classes.join(' ') + ' ' + this.classList;
    }

    copyToClipboard(val: string) {
        const myWindow: any = window;

        const onCopy = (e: ClipboardEvent) => {
            e.preventDefault();

            if (e.clipboardData) {
                e.clipboardData.setData('text/plain', val);
            } else if (myWindow.clipboardData) {
                myWindow.clipboardData.setData('Text', val);
            }

            myWindow.removeEventListener('copy', onCopy);
        };

        myWindow.addEventListener('copy', onCopy);

        if (myWindow.clipboardData && myWindow.clipboardData.setData) {
            myWindow.clipboardData.setData('Text', val);
        } else {
            document.execCommand('copy');
        }
    }

    doCopy() {
        this.copyToClipboard(this.value);
        // this.firstLoad = false;
        this.animClasses = 'flip-horizontal-bottom';
        setTimeout(() => {
            this.animClasses = 'flip-horizontal-reverse';
        }, SHOW_CHECKBOX_TIMEOUT);
    }
}
