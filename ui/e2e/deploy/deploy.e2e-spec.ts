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
    authHelper.deleteSessionData();
  });

  beforeEach(() => {
    appPage = new AppPage();
    dashboardPage = new DashboardPage();
    onboardingPage = new OnboardingPage();
    deployWiz = new DeployWizard();
    browser.waitForAngularEnabled(false);
  });

  it('should login', () => {
    loginPage = new CSPLogin();
    loginPage.navigateTo();
    waitForURLContains('console-stg.cloud.vmware.com/csp/gateway/discovery');
    browser.sleep(500);
    loginPage.fillInEmail();
    waitForURLContains('csp-local.vidmpreview.com/SAAS/auth/login');
    browser.sleep(1000);
    loginPage.fillInPassword();
  });

  it('should accept terms of service', () => {
    browser.sleep(2000);
    onboardingPage.readAndClickAccept('Reinhard', 'von Lohengramm', 'Galactic Empire');
    browser.sleep(1500);
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

    expect(deployWiz.getTitle()).toBe(title);
    expect(deployWiz.getEngine()).toBe('DAML');
    expect(deployWiz.getNumberOfReplicas()).toBe('4');

    deployWiz.deploy();
  });

  it('should show progress of the blockchain', () => {
    const progressEl = '#deployProgress';
    const progMessageEl = `${progressEl} h5`;
    const progPerceEl = `${progressEl} .progress span`;
    waitFor(progressEl);
    waitForText(element(by.cssContainingText(progMessageEl, `Creating VM's...`)));
    waitForText(element(by.cssContainingText(progMessageEl, `Deploying concord replicas...`)));
    waitForText(element(by.cssContainingText(progPerceEl, `100%`)));
    expect(deployWiz.getPercentage()).toBe('100%');
    browser.sleep(4000);
  });

  it('should do the DAML tour', () => {
    browser.sleep(1000);
    expect(appPage.getTourTitle().getText()).toEqual('General Status');
    browser.sleep(300);
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Committer List');
    appPage.getTourNextButton().click();
    browser.sleep(300);
    expect(appPage.getTourTitle().getText()).toEqual('Organizations');
    appPage.clickTourEndButton();
    browser.sleep(300);
    expect(appPage.getTourTitle().isDisplayed()).toBe(false);
    browser.waitForAngularEnabled(true);
  });

  it('should switch to the default blockchain', () => {
    dashboardPage.switchToDefault();
    waitForText(element(by.cssContainingText('.nav-text', 'Smart Contracts')));
  });

});
