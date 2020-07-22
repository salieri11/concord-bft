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
    const message = this.startMessaging(taskId).subscribe();

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

  startMessaging(taskId: string): Observable<any> {
    const multiplier = this.blockchainType === ContractEngines.ETH ? 7 : 15;
    const duration = 1000 * multiplier;
    const tracker = this.blockchainService.loadDeployingData(taskId);
    const previousPercentage = tracker.at ? tracker.at : 0;

    this.loop = true;
    this.message = this.translate.instant('deployLoader.initial');

    // Helper function
    const saveProgress = () => {
      tracker.at = this.progress;
      this.blockchainService.saveDeployingData(tracker);
    };

    return of(true).pipe(
      delay(previousPercentage < 10 ? duration : 0),
      map(response => {
        this.loop = false;
        this.progress = 10; saveProgress();
        this.message = this.translate.instant('deployLoader.first');
        return response;
      }),
      delay(previousPercentage < 12 ? duration : 0),
      map(() => {this.progress = 12; saveProgress(); }),
      delay(previousPercentage < 13 ? duration / 2 : 0),
      map(() => {this.progress = 13; saveProgress(); }),
      delay(previousPercentage < 16 ? duration / 2 : 0),
      map(() => {this.progress = 16; saveProgress(); }),
      delay(previousPercentage < 18 ? duration : 0),
      map(() => {this.progress = 18; saveProgress(); }),
      delay(previousPercentage < 25 ? duration : 0),
      map(() => {
        this.progress = 25; saveProgress();
        this.message = this.translate.instant('deployLoader.second');
      }),
      delay(previousPercentage < 30 ? duration : 0),
      map(() => {this.progress = 30; saveProgress(); }),
      delay(previousPercentage < 34 ? duration : 0),
      map(() => {this.progress = 34; saveProgress(); }),
      delay(previousPercentage < 36 ? duration : 0),
      map(() => {this.progress = 36; saveProgress(); }),
      delay(previousPercentage < 37 ? duration / 2 : 0),
      map(() => {this.progress = 37; saveProgress(); }),
      delay(previousPercentage < 39 ? duration / 2 : 0),
      map(() => {this.progress = 39; saveProgress(); }),
      delay(previousPercentage < 40 ? duration / 2 : 0),
      map(() => {
        this.progress = 40; saveProgress();
        this.message = this.translate.instant('deployLoader.third');
      }),
      delay(previousPercentage < 43 ? duration : 0),
      map(() => {this.progress = 43; saveProgress(); }),
      delay(previousPercentage < 45 ? duration : 0),
      map(() => {
        this.progress = 45; saveProgress();
        this.message = this.translate.instant('deployLoader.fourth');
      }),
      delay(previousPercentage < 45 ? duration : 0),
      map(() => {this.progress = 45; saveProgress(); }),
      delay(previousPercentage < 50 ? duration / 2 : 0),
      map(() => {this.progress = 50; saveProgress(); }),
      delay(previousPercentage < 53 ? duration / 2 : 0),
      map(() => {this.progress = 53; saveProgress(); }),
      delay(previousPercentage < 60 ? duration : 0),
      map(() => {this.progress = 60; saveProgress(); }),
      delay(previousPercentage < 62 ? duration / 2 : 0),
      map(() => {this.progress = 62; saveProgress(); }),
      delay(previousPercentage < 72 ? duration / 2 : 0),
      map(() => {this.progress = 72; saveProgress(); }),
      delay(previousPercentage < 80 ? duration / 2 : 0),
      map(() => {this.progress = 80; saveProgress(); }),
      delay(previousPercentage < 85 ? duration : 0),
      map(() => {this.progress = 85; saveProgress(); }),
      delay(duration),
    );

  }

  private showError(error: HttpResponse<any> | HttpErrorResponse) {
    this.error = error['message'] || error['error_message'];
    this.loading = false;
  }

}
