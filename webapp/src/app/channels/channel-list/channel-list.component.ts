
import {
	Component,
	OnInit,
	Input,
	Output,
	EventEmitter,
} from '@angular/core';
import { GridOptions, GridColumn } from '../../grid/shared/grid.model';
import { TranslateService } from '@ngx-translate/core';
import { ChannelService } from '../shared/channel.service';

import { Channel } from '../shared/channel.model';


@Component({
  selector: 'fb-channel-list',
  templateUrl: './channel-list.component.html',
  styleUrls: ['./channel-list.component.scss']
})
export class ChannelListComponent implements OnInit {
	@Input('url') url?: string;

  constructor() { }

  ngOnInit() {
  }

}
