import { Component, OnInit } from '@angular/core';

import {AthenaApiService} from "../../athena-api.service";

@Component({
  selector: 'app-transaction-list-view',
  templateUrl: './transaction-list-view.component.html',
  styleUrls: ['./transaction-list-view.component.scss']
})
export class TransactionListViewComponent implements OnInit {

  recentTransactions: any[] = [];

  constructor(private athenaApiService: AthenaApiService) { }

  ngOnInit() {
    this.athenaApiService.getRecentTransactions().subscribe((resp) => {
      this.recentTransactions = resp;
    });
    console.log(this.recentTransactions);
  }

}
