/*
 * Copyright 2019-2020 VMware, all rights reserved.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { ClrDatagrid } from '@clr/angular';
import { Task } from '../shared/task.model';
import { TasksService } from '../shared/tasks.service';

@Component({
  selector: 'concord-task-list',
  templateUrl: './task-list.component.html',
  styleUrls: ['./task-list.component.scss']
})
export class TaskListComponent implements OnInit {
  @ViewChild('grid', {static: false}) grid: ClrDatagrid;

  tasks: Task[];
  selected: any[] = [];
  loading: boolean;

  constructor(
    private tasksService: TasksService
  ) { }

  ngOnInit() {
    this.loadTasks();
  }

  loadTasks() {
    this.tasksService.getList()
      .subscribe(tasks => {
        this.tasks = tasks.tasks;
        this.loading = false;
      }, () => this.loading = false);
  }
}
