/*
 * Copyright 2019 VMware, all rights reserved.
 */

import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'concord-error',
  templateUrl: './error.component.html',
  styleUrls: ['./error.component.scss']
})
export class ErrorComponent implements OnInit {
  title: string;
  status: number;
  message: string;
  constructor(private route: ActivatedRoute) { }

  ngOnInit() {
    // Wait for route to resolve to get state
    this.route.queryParams.subscribe(params => {
        const error = JSON.parse(params.error);

        this.title = error.name || 'Error';
        this.status = error.status;
        this.message = error.message || error.error.error_message;
    });
  }

}
