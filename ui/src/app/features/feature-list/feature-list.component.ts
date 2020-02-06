/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';
import { Feature } from '../shared/feature.model';
// import { FeaturesService } from '../shared/feartures.service';

@Component({
  selector: 'concord-feature-list',
  templateUrl: './feature-list.component.html',
  styleUrls: ['./feature-list.component.scss']
})
export class FeaturesListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;

  features: Feature[];
  selected: any[] = [];
  loading: boolean;

  constructor(
    // private featuresService: FeaturesService
  ) { }

  ngOnInit() {
    this.loadFeatures();
  }

  loadFeatures() {
    // this.tasksService.getTasks()
    //   .subscribe(features => this.features = features.features);
  }
}
