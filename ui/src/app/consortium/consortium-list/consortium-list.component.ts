/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';
import { ConsortiumResponse } from '../shared/consortium.model';
import { ConsortiumService } from '../shared/consortium.service';

@Component({
  selector: 'concord-consortium-list',
  templateUrl: './consortium-list.component.html',
  styleUrls: ['./consortium-list.component.scss']
})
export class ConsortiumListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;

  consortiums: ConsortiumResponse[];
  selected: any[] = [];
  loading: boolean;

  constructor(
    private consortiumsService: ConsortiumService
  ) { }

  ngOnInit() {
    this.loadConsortiums();
  }

  loadConsortiums() {
    this.consortiumsService.getList()
      .subscribe(consortiums => {
        this.consortiums = consortiums;
        this.loading = false;
      }, () => this.loading = false);
  }
}
