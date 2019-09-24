/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

import { AuthenticationService } from '../../shared/authentication.service';
import { ErrorAlertService } from '../../shared/global-error-handler.service';
import { User } from '../shared/user.model';
import { UsersService } from '../shared/users.service';
import { generateDownload } from '../../shared/download-helpers';
import { WalletFormComponent } from '../wallet-form/wallet-form.component';
import { EthWallet } from '../../shared/eth-api.model';

@Component({
  selector: 'concord-user-settings',
  templateUrl: './user-settings.component.html',
  styleUrls: ['./user-settings.component.scss']
})
export class UserSettingsComponent implements OnInit {
  @ViewChild('walletForm', { static: true }) walletForm: WalletFormComponent;
  user: User;
  userWallet: EthWallet = null;

  constructor(
    private errorService: ErrorAlertService,
    private authenticationService: AuthenticationService,
    private translateService: TranslateService,
    private usersService: UsersService
  ) {
    this.fetchUser(this.authenticationService.currentUser.user_id);
    this.fetchWallet(this.authenticationService.currentUser.user_id);
  }

  ngOnInit() {
  }

  get hasWallet() {
    return this.userWallet != null;
  }

  fetchUser(userId: string) {
    this.usersService.getUser(userId).subscribe((resp) => {
      this.user = resp;
    }, () => {
      this.errorService.add(new Error(this.translateService.instant('users.settings.errors.fetchUserError')));
    });
  }

  fetchWallet(userId: string) {
    const walletAddress = this.authenticationService.currentUser.wallet_address.slice(2);
    if (walletAddress !== '') {
      this.usersService.getUserWalletByAddress(userId, walletAddress).subscribe((walletResp) => {
        this.userWallet = walletResp;
      }, () => {
        this.errorService.add(new Error(this.translateService.instant('users.settings.errors.fetchWalletError')));
      });
    }
  }

  onCreatedWallet(userWallet) {
    this.userWallet = userWallet;
    localStorage['helen.wallet_address'] = `0x${this.userWallet.address}`;
  }

  onDownloadWallet() {
    generateDownload(`${this.userWallet.address}-wallet.json`, JSON.stringify(this.userWallet, null, 4));
  }
}
