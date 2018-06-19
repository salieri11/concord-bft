/*
 * Copyright 2018 VMware, all rights reserved.
 */
import {
  Component,
  OnInit,
  Input,
} from '@angular/core';

@Component({
  selector: 'app-channel-list',
  templateUrl: './channel-list.component.html',
  styleUrls: ['./channel-list.component.scss']
})
export class ChannelListComponent implements OnInit {
  @Input('url') url?: string;

  constructor() { }

  ngOnInit() {
  }

}
