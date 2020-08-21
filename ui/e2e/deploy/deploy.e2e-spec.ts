/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { browser, element, by } from 'protractor';

import { AuthHelper } from '../helpers/auth';
import { AppPage } from '../app/app.po';
import { LoginPage, CSPLogin } from '../login/login.po';
import { DashboardPage } from '../dashboard/dashboard.po';
import { OnboardingPage } from '../onboarding/onboarding.po';
import { DeployWizard } from './deploy.po';
import { waitFor, waitForText, waitForURLContains } from '../helpers/utils';

describe('concord-ui Deployment Flow', () => {
  let authHelper: AuthHelper;
  let appPage: AppPage;
  let deployWiz: DeployWizard;
  let dashboardPage: DashboardPage;
  let onboardingPage: OnboardingPage;
  let loginPage: CSPLogin;

  afterEach(() => {
    authHelper = new AuthHelper();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    onboardingPage = new OnboardingPage();
    deployWiz = new DeployWizard();
    browser.waitForAngularEnabled(false);
  });

  it('should deploy a blockchain', () => {
    const title = 'DAML e2e';

    browser.sleep(2000);
    appPage.goToDeployWizard().click();
    browser.sleep(800);
    deployWiz.selectDaml();
    browser.sleep(100);
    deployWiz.next();
    browser.sleep(100);
    deployWiz.consortiumTitleInput().sendKeys(title);
    browser.sleep(100);
    deployWiz.next();
    browser.sleep(100);
    deployWiz.selectFourReplicas();
    browser.sleep(100);
    deployWiz.next();
    browser.sleep(100);
    deployWiz.getClientGroupName().sendKeys('test');
    browser.sleep(100);
    deployWiz.next();
    browser.sleep(100);

    expect(deployWiz.getTitle()).toBe(title);
    expect(deployWiz.getEngine()).toBe('DAML');
    expect(deployWiz.getNumberOfReplicas()).toBe('4');

    deployWiz.deploy();
  });

  it('should show progress of the blockchain', () => {
    const progressEl = '#deployProgress';
    const progMessageEl = '.deploy-message';
    const progPerceEl = '.progress-label';
    waitFor(progressEl);
    waitForText(element(by.cssContainingText(progMessageEl, `Deploy consortium started...`)));
    waitForText(element(by.cssContainingText(progMessageEl, `Creating VMs...`)));
    waitForText(element(by.cssContainingText(progMessageEl, `Deploying concord committers...`)));
    waitForText(element(by.cssContainingText(progPerceEl, `100%`)));
    expect(deployWiz.getPercentage()).toBe('100%');
    browser.sleep(10000);
  });

  it('should be a healthy network', () => {
    browser.sleep(1000);
    expect(deployWiz.getHealthNumber()).toEqual('4/4');
  });

  it('should switch to the default blockchain', () => {
    dashboardPage.switchToDefault();
    waitForText(element(by.cssContainingText('.nav-text', 'Smart Contracts')));
  });

});
