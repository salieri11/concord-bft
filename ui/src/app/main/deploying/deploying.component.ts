/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild, OnDestroy } from '@angular/core';
import { DeployingInterstitialComponent } from '../deploying-interstitial/deploying-interstitial.component';
import { Subscription } from 'rxjs';
import { ActivatedRoute, Router } from '@angular/router';
import { uuidRegExp, mainRoutes, ConsortiumStates } from '../../shared/urls.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { DeployStates } from '../../blockchain/shared/blockchain.model';

import { RouteService } from '../../shared/route.service';

@Component({
  selector: 'concord-deploying',
  templateUrl: './deploying.component.html',
  styleUrls: ['./deploying.component.scss']
})
export class DeployingComponent implements OnInit, OnDestroy {

  @ViewChild('deployLoader', { static: true }) deployLoader: DeployingInterstitialComponent;
  routingSub: Subscription;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private blockchainService: BlockchainService,
    private routeService: RouteService,
    ) {
  }

  async ngOnInit() {

    this.routingSub = this.route.params.subscribe(async param => {
      const taskId = param.taskId as string;

      if (!taskId) {
        const runningTasks = await this.getRunningTasks();
        if (runningTasks.length === 0) {
          return this.routeService.redirectToDefault();
        }
        return this.routeService.goToDeploying(runningTasks[0].task_id);
      }

      /**
       * Fleeting state to at least show progress UI until deploy request returns with :taskId
       * Something is wrong if stuck here for 10+ sec; in that case, try redirect to dash.
       * */
      if (taskId === ConsortiumStates.waiting) {
        const waitTime = 10 * 1000;
        setTimeout(() => {
          if (this.router.url === '/' + mainRoutes.deployingWaitingURL) {
            this.routeService.redirectToDefault();
          }
        }, waitTime);
        return;
      }

      const task = await this.getTask(taskId);
      if (!task || task.state === DeployStates.SUCCEEDED) {
        return this.routeService.redirectToDefault();
      }

      this.deployLoader.startLoading(taskId);
    });
  }

  async getRunningTasks() {
    let tasks;
    try {
      tasks = await this.blockchainService.getTasks().toPromise();
    } catch (e) { console.log(e.error); return []; }

    if (!tasks) { return []; }
    const runningTasks = tasks.filter(item => (item.state === DeployStates.RUNNING));
    if (runningTasks.length === 0) { return []; }
    return runningTasks;
  }

  async getTask(taskId: string): Promise<any> {
    if (!uuidRegExp.test(taskId)) { return null; }
    let task;
    try {
      task = await this.blockchainService.getTask(taskId).toPromise();
    } catch (e) { console.log(e.error); return null; }
    if (!task) { return null; }
    return task;
  }

  ngOnDestroy () {
    if (this.routingSub) { this.routingSub.unsubscribe(); }
  }

}
