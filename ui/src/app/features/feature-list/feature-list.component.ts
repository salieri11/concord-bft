/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';
// import { Feeature } from '../shared/fearture.model';
// import { FeaturesService } from '../shared/feartures.service';

@Component({
  selector: 'concord-feature-list',
  templateUrl: './feature-list.component.html',
  styleUrls: ['./feature-list.component.scss']
})
export class FeaturesListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;

  // tasks: Task[];
  selected: any[] = [];

  constructor(
    // private featuresService: FeaturesService
  ) { }

  ngOnInit() {
    this.loadNodes();
  }

  loadNodes() {
    // this.tasksService.getTasks()
    //   .subscribe(nodes => this.nodes = nodes.nodes);
  }
}
