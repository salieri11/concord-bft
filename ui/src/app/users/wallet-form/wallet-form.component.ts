/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { ClrWizard } from '@clr/angular';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Accounts as Web3EthAccounts } from 'web3-eth-accounts';

import { EthApiService } from '../../shared/eth-api.service';
import { UsersService } from '../shared/users.service';
import { User } from '../shared/user.model';
import { EthWallet } from '../../shared/eth-api.model';

@Component({
  selector: 'concord-wallet-form',
  templateUrl: './wallet-form.component.html',
  styleUrls: ['./wallet-form.component.scss']
})
export class WalletFormComponent implements OnInit {
  @Input('hasWallet') hasWallet: boolean;
  @Input('user') user: User;
  @Input('userWallet') userWallet: EthWallet = null;
  @Output() createdWallet: EventEmitter<EthWallet> = new EventEmitter<EthWallet>();
  @ViewChild('wizard', { static: true }) wizard: ClrWizard;
  createWalletForm: FormGroup;
  decryptedWallet: any = null;
  decryptError: string = null;
  isOpen = false;
  walletLoading = false;
  constructor(private usersService: UsersService,
              private ethService: EthApiService) {
    this.createWalletForm = new FormGroup({
      password: new FormControl('', Validators.required)
    });
  }

  ngOnInit() {
  }

  public open() {
    this.wizard.reset();
    this.decryptedWallet = null;
    this.createWalletForm.reset();
    this.isOpen = true;
  }

  onSubmitCreate() {
    this.decryptError = null;
    this.walletLoading = true;
    this.wizard.next();
    this.ethService.createWallet(this.createWalletForm.value.password).subscribe((resp) => {
      this.usersService.getUserWalletByAddress(this.user.user_id, resp.result.slice(2)).subscribe((walletResp) => {
        this.createdWallet.emit(walletResp);
        this.walletLoading = false;
      });
    });
  }

  onSubmitView() {
    this.decryptError = null;
    this.walletLoading = true;
    this.wizard.next();

    setTimeout(() => {
      // The decrypt operation takes some time. The timing for showing the loader and moving to the next page
      // was happening in the same zone, so the UI never updated. Wrapping in a timeout makes it work
      try {
        this.decryptWallet(this.userWallet, this.createWalletForm.value.password);
      } catch (err) {
        this.decryptError = err.message;
      } finally {
        this.walletLoading = false;
      }
    }, 100);
  }

  private decryptWallet(wallet, password) {
    const account = new Web3EthAccounts();
    this.decryptedWallet = account.wallet.decrypt([wallet], password);
  }
}
