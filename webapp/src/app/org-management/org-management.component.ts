import {
  Component,
  OnInit,
  ViewChild,
} from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { OrgListComponent } from './org-list/org-list.component';


@Component({
  selector: 'app-org-management',
  templateUrl: './org-management.component.html',
  styleUrls: ['./org-management.component.scss']
})
export class OrgManagementComponent implements OnInit {
  @ViewChild('orgList') orgList: OrgListComponent;


  constructor(private translate: TranslateService,) {
    const browserLang = translate.getBrowserLang();
    translate.setDefaultLang('en');
    translate.use(browserLang);
  }

  ngOnInit() {
  }
}
