/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { MainComponent } from './main.component';
import { getSpecTestingModule } from '../../shared/shared-testing.module';
import { mockBlockchains } from '../../blockchain/shared/blockchain.model';
import { RouteService } from '../../shared/route.service';
import { mainFragments } from '../../shared/urls.model';
import { MockBlockchainService, BlockchainService } from '../../blockchain/shared/blockchain.service';


describe('MainComponent', () => {
  const tester = getSpecTestingModule();
  let routeService: RouteService;
  let blockchainService: MockBlockchainService;
  let component: MainComponent;
  let fixture: ComponentFixture<MainComponent>;
  const resetComponent = () => {
    fixture = TestBed.createComponent(MainComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  };

  beforeEach(async(() => {
    TestBed.configureTestingModule(tester.init({
      imports: [], provides: [], declarations: []
    })).compileComponents();
    tester.dynamicActivatedRoute();
    routeService = tester.getService(RouteService);
    blockchainService = tester.getService(BlockchainService);
  }));

  it('should create', () => {
    resetComponent();
    expect(component).toBeTruthy();
  });

  it('with no blockchain, should redirect to default', () => {
    const routeRedirectFunc = spyOn(routeService, 'redirectToDefault');
    resetComponent();
    expect(routeRedirectFunc).toHaveBeenCalledWith();
  });

  it('no wrong blockchain id, should redirect to default', () => {
    const routeRedirectFunc = spyOn(routeService, 'redirectToDefault');
    tester.updateActivatedRoute({ blockchainId: 'test' }); // won't pass UUID test
    resetComponent();
    expect(routeRedirectFunc).toHaveBeenCalledWith();
  });

  it('Welcome modal should open on fragment change to welcome', () => {
    tester.updateActivatedRoute({
      blockchainId: mockBlockchains[0].id, fragment: null // valid main
    });
    resetComponent();
    const welcomeModalOpen = spyOn(component.welcomeModal, 'open');
    tester.activatedRoute.fragment.next(mainFragments.welcome);
    expect(welcomeModalOpen).toHaveBeenCalledWith();
  });

  describe('mock blockchains', () => {
    it('should provide mock blockchains', () => {
      // 3 mock blockchains provided (ETH, DAML, ETH-FAILED)
      blockchainService.provideMockBlockchains();
      resetComponent(); // re-provide blockchainService
      expect(component.blockchains.length).toBe(mockBlockchains.length);
    });
    it('should provide Swagger-generated blockchains', async () => {
      await blockchainService.provideMockBlockchainsFromSwagger();
      resetComponent(); // re-provide blockchainService
      // Swagger generated GET /blockchains has only 1 example
      expect(component.blockchains.length).toBe(1);
    });
  });

});
