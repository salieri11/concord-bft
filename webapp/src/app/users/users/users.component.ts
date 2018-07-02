/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';

import { Personas } from '../../shared/persona.service';
import { UsersListComponent } from './users-list/users-list.component';

@Component({
  selector: 'athena-users',
  templateUrl: './users.component.html',
  styleUrls: ['./users.component.scss']
})
export class UsersComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin];
  @ViewChild('usersList') usersList: UsersListComponent;

  constructor() { }

  ngOnInit() { }

}
