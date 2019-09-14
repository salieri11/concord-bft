/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { HttpResponse, HttpErrorResponse } from '@angular/common/http';
import { TranslateService } from '@ngx-translate/core';
import { of, Observable } from 'rxjs';
import { delay, map } from 'rxjs/operators';

import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { DeployStates, ContractEngines } from '../../blockchain/shared/blockchain.model';

@Component({
  selector: 'concord-deploying-interstitial',
  templateUrl: './deploying-interstitial.component.html',
  styleUrls: ['./deploying-interstitial.component.scss']
})
export class DeployingInterstialComponent {
  loading: boolean = true;
  title: string = 'Deploy Consortium';
  error: string;
  showInterstitial = false;
  message: string = 'Deploy consortium started...';
  loop: boolean = true;
  success: boolean = false;
  progress: number = 0;
  blockchainType: ContractEngines;

  constructor(
    private blockchainService: BlockchainService,
    private router: Router,
    private translate: TranslateService
  ) {
    this.blockchainService.notify.subscribe(message => {
      if (message && message.message === 'deploying') {
        this.loading = true;
        this.showInterstitial = true;
        this.blockchainType = message.type;
      }
    });

    // this.startMessaging().subscribe(() => {
    //   console.log('done');
    // });
  }

  startLoading(response: HttpResponse<any> | HttpErrorResponse) {
    this.error = null;
    this.loading = true;
    this.showInterstitial = true;

    if (response && response['task_id']) {
      this.pollUntilDeployFinished(response['task_id']);
    } else {
      this.showError(response);
    }
  }

  private pollUntilDeployFinished(taskId: string) {
    const message = this.startMessaging().subscribe();

    this.blockchainService.pollDeploy(taskId)
      .subscribe((response) => {

        if (response.state === DeployStates.SUCCEEDED) {
          message.unsubscribe();
          this.progress = 100;
          this.message = 'Deployment successful!';

          this.blockchainService.set().subscribe(() => {
            setTimeout(() => {
              // TODO: enable the dashboard to show a toast - response.resource_id is the bid
              this.router.navigate([`/${response.resource_id}`, 'dashboard'], {fragment: 'orgTour'});
              this.showInterstitial = false;
              this.blockchainService.notify.next({message: 'deployed'});
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
    this.title = this.translate.instant('error.title');
    this.error = error['message'] || error['error_message'];
    this.loading = false;
  }
}
