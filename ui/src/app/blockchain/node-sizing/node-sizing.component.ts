/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Output, EventEmitter } from '@angular/core';
import { NodeTemplates, NodeTemplate, NodeTemplateFormResponse } from '../../nodes/shared/nodes.model';
import { NodesService } from '../../nodes/shared/nodes.service';
import {
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';

@Component({
  selector: 'concord-node-sizing',
  templateUrl: './node-sizing.component.html',
  styleUrls: ['./node-sizing.component.scss']
})
export class NodeSizingComponent {
  @Output() sizeChange = new EventEmitter<NodeTemplateFormResponse>();
  @Output() isValid = new EventEmitter<boolean>();

  nodeSizing: NodeTemplate;
  sizingOptions: NodeTemplates;
  selectedSizing: string;
  showTemplates: boolean = true;
  form: FormGroup;
  range: any;
  hasClients: any;

  constructor(private nodesService: NodesService) {
    this.nodesService.getSizingOptions().subscribe(options => this.init(options));
  }

  init(options: NodeTemplates): void {
    this.sizingOptions = options;
    this.hasClients = options.templates[0].items.some(a => a.type === 'client');
    this.form = this.initForm();
    this.form.valueChanges.subscribe(() => {
      this.isValid.emit(this.isValidSelection());

      if (this.form.valid) {
        const value = this.form.value;
        // Convert numbers to string for API
        // Useful to keep as numbers in form so that we can use built in
        // validators
        for (const [key, val] of Object.entries(value.committerSizing)) {
          value.committerSizing[key] = val.toString();
        }
        for (const [key, val] of Object.entries(value.clientSizing)) {
          value.clientSizing[key] = val.toString();
        }
        this.sizeChange.emit(value);
      }
    });
  }

  selectSize(type: string) {
    this.selectedSizing = type;
    const items = this.sizingOptions.templates.filter(item => item.name === type)[0].items;

    this.sizeChange.emit({
      committerSizing: items[0],
      clientSizing: items[1]
    });
    this.isValid.emit(this.isValidSelection());
  }

  toggleTemplatesCustom() {
    this.showTemplates = !this.showTemplates;
    this.form.reset();
    this.selectedSizing = undefined;
    this.isValid.emit(this.isValidSelection());
  }

  private isValidSelection(): boolean {
    return this.form.valid || this.selectedSizing !== undefined;
  }

  private initForm(): FormGroup {
    this.range = this.sizingOptions.range;
    return new FormGroup({
      committerSizing: new FormGroup({
        no_of_cpus: new FormControl(
          this.range.no_of_cpus.min, [
          Validators.required,
          Validators.min(this.range.no_of_cpus.min),
          Validators.max(this.range.no_of_cpus.max),
        ]),
        memory_in_gigs: new FormControl(
          this.range.memory_in_gigs.min,
          [
          Validators.required,
          Validators.min(this.range.memory_in_gigs.min),
          Validators.max(this.range.memory_in_gigs.max),
        ]),
        storage_in_gigs: new FormControl(
          this.range.storage_in_gigs.min, [
          Validators.required,
          Validators.min(this.range.storage_in_gigs.min),
          Validators.max(this.range.storage_in_gigs.max),
        ])
      }),
      clientSizing: new FormGroup({
        no_of_cpus: new FormControl(
          this.range.no_of_cpus.min, [
          Validators.required,
          Validators.min(this.range.no_of_cpus.min),
          Validators.max(this.range.no_of_cpus.max),
        ]),
        memory_in_gigs: new FormControl(
          this.range.memory_in_gigs.min,
          [
          Validators.required,
          Validators.min(this.range.memory_in_gigs.min),
          Validators.max(this.range.memory_in_gigs.max),
        ]),
        storage_in_gigs: new FormControl(
          this.range.storage_in_gigs.min, [
          Validators.required,
          Validators.min(this.range.storage_in_gigs.min),
          Validators.max(this.range.storage_in_gigs.max),
        ])
      })
    });
  }

}
