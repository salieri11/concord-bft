/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, ViewChild, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { HttpResponse, HttpErrorResponse } from '@angular/common/http';
import { TranslateService } from '@ngx-translate/core';
import { of, Observable, Subscription } from 'rxjs';
import { delay, map } from 'rxjs/operators';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { DeployStates, ContractEngines } from '../../blockchain/shared/blockchain.model';
import { ClrProgressBar } from '@clr/angular';

@Component({
  selector: 'concord-deploying-interstitial',
  templateUrl: './deploying-interstitial.component.html',
  styleUrls: ['./deploying-interstitial.component.scss']
})
export class DeployingInterstitialComponent implements OnDestroy {

  @ViewChild('progressBar', { static: true }) progressBar: ClrProgressBar;

  loading: boolean = true;
  error: string;
  showInterstitial = false;
  message: string;
  loop: boolean = true;
  success: boolean = false;
  isOnlyOnPrem: boolean = false;
  progress: number = 0;
  blockchainType: ContractEngines;

  blockchainNotifySub: Subscription;

  constructor(
    private blockchainService: BlockchainService,
    private router: Router,
    private translate: TranslateService
  ) {
    this.blockchainNotifySub = this.blockchainService.notify.subscribe(e => {
      if (e && e.message === 'deploying') {
        this.loading = true;
        this.blockchainType = e.type;
        this.isOnlyOnPrem = e.isOnlyOnPrem;
      }
    });
    this.message = this.translate.instant('deployLoader.waiting');
  }

  startLoading(taskId: string, error?: HttpErrorResponse) {
    if (error) {
      this.showError(error);
    } else {
      this.pollUntilDeployFinished(taskId);
    }

    this.error = null;
    this.loading = true;
  }

  ngOnDestroy() {
    if (this.blockchainNotifySub) { this.blockchainNotifySub.unsubscribe(); }
  }

  private pollUntilDeployFinished(taskId: string) {
    const message = this.startMessaging().subscribe();

    this.blockchainService.pollDeploy(taskId)
      .subscribe((response) => {
        if (response.state === DeployStates.SUCCEEDED) {
          message.unsubscribe();
          this.progress = 100;
          this.message = this.translate.instant('deployLoader.ending');
          const deployedBlockchainId = response.resource_id;
          this.blockchainService.set(deployedBlockchainId).subscribe(() => {
            setTimeout(() => {
              const fragment = this.isOnlyOnPrem ? null : 'orgTour';
              // TODO: enable the dashboard to show a toast - response.resource_id is the bid
              this.router.navigate([`/${deployedBlockchainId}`, 'dashboard'], {fragment: fragment});
              this.blockchainService.notify.next({message: 'deployed', blockchainId: deployedBlockchainId});
            }, 3000);
          });
        } else {
          this.showError(response);
        }
      }, error => {
         this.showError(error);
      });
  }

  startMessaging(): Observable<any> {
    const multiplier = this.blockchainType === ContractEngines.ETH ? 7 : 15;
    const duration = 1000 * multiplier;

    this.loop = true;
    this.message = this.translate.instant('deployLoader.initial');

    return of(true).pipe(
      delay(duration),
      map(response => {
        this.loop = false;
        this.progress = 10;
        this.message = this.translate.instant('deployLoader.first');
        return response;
      }),
      delay(duration),
      map(() => {this.progress = 12; }),
      delay(duration / 2),
      map(() => {this.progress = 13; }),
      delay(duration / 2),
      map(() => {this.progress = 16; }),
      delay(duration),
      map(() => {this.progress = 18; }),
      delay(duration),
      map(() => {
        this.progress = 25;
        this.message = this.translate.instant('deployLoader.second');
      }),
      delay(duration),
      map(() => {this.progress = 30; }),
      delay(duration),
      map(() => {this.progress = 34; }),
      delay(duration),
      map(() => {this.progress = 36; }),
      delay(duration / 2),
      map(() => {this.progress = 37; }),
      delay(duration / 2),
      map(() => {this.progress = 39; }),
      delay(duration / 2),
      map(() => {
        this.progress = 40;
        this.message = this.translate.instant('deployLoader.third');
      }),
      delay(duration),
      map(() => {this.progress = 43; }),
      delay(duration),
      map(() => {
        this.progress = 45;
        this.message = this.translate.instant('deployLoader.fourth');
      }),
      delay(duration),
      map(() => {this.progress = 45; }),
      delay(duration / 2),
      map(() => {this.progress = 50; }),
      delay(duration / 2),
      map(() => {this.progress = 53; }),
      delay(duration),
      map(() => {this.progress = 60; }),
      delay(duration / 2),
      map(() => {this.progress = 62; }),
      delay(duration / 2),
      map(() => {this.progress = 72; }),
      delay(duration / 2),
      map(() => {this.progress = 80; }),
      delay(duration),
      map(() => {this.progress = 85; }),
      delay(duration),
    );

  }

  private showError(error: HttpResponse<any> | HttpErrorResponse) {
    this.error = error['message'] || error['error_message'];
    this.loading = false;
  }

}
