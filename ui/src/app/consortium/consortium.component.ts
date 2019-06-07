/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  OnInit,
} from '@angular/core';

import { Personas } from '../shared/persona.service';
import { Consortium } from './shared/consortium.model';

@Component({
  selector: 'concord-consortium',
  templateUrl: './consortium.component.html',
  styleUrls: ['./consortium.component.scss']
})
export class ConsortiumComponent implements OnInit {
  static personasAllowed: Personas[] = [Personas.SystemsAdmin, Personas.ConsortiumAdmin];

  selected: Array<Consortium>;

  constructor() {}

  ngOnInit() {}

}
