/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

// import { MainComponent } from './main.component';
// import { testFor, beforeTesting, prepareEach } from '../../../test.helper.spec';
// import { mockBlockchains } from '../../blockchain/shared/blockchain.model';
// import { RouteService } from '../../shared/route.service';
// import { mainFragments, uuidRegExp, mainRoutes } from '../../shared/urls.model';
// import { MockBlockchainService, BlockchainService } from '../../blockchain/shared/blockchain.service';
// import { swaggerMocks } from '../../../test.swagger.spec';
// import { CanViewDirective } from '../../shared/directives/can-view.directive';
// import { FeatureFlagDirective } from '../../shared/directives/feature-flag.directive';
// import { FormsModule } from '@angular/forms';
// import { ClarityModule } from '@clr/angular';

// describe('MainComponent', () => {
//   let routeService: RouteService;
//   let blockchainService: MockBlockchainService;

//   const test = testFor(MainComponent).expedite({
//     imports: [ClarityModule, FormsModule], provides: [],
//     declarations: [MainComponent, CanViewDirective, FeatureFlagDirective],
//     router: `/${mockBlockchains[0].id}/${mainRoutes.dashboard}`,
//     route: { // default valid route
//       url: [{
//         path: mockBlockchains[0].id,
//         params: { blockchainId: mockBlockchains[0].id }
//       }]
//     }
//   }, beforeTesting(() => {
//     routeService = test.getService(RouteService);
//     blockchainService = test.getService(BlockchainService);
//   }), prepareEach(() => {
//     blockchainService.provideMockBlockchains();
//   }));

//   it('should create', () => {
//     test.createComponent();
//     expect(test.component).toBeTruthy();
//   });

//   it('with no blockchain created, should redirect to default', () => {
//     const routeRedirectFunc = spyOn(routeService, 'redirectToDefault');
//     blockchainService.provideNoBlockchain();
//     test.refreshComponent();
//     expect(routeRedirectFunc).toHaveBeenCalledWith();
//   });

//   it('wrong blockchain id should redirect to default', () => {
//     test.router.url = `/test/${mainRoutes.dashboard}`; // won't pass UUID test
//     const routeRedirectFunc = spyOn(routeService, 'redirectToDefault');
//     test.refreshComponent();
//     expect(routeRedirectFunc).toHaveBeenCalledWith();
//   });

//   it('should redirect bad URLs regardless of child routes', () => {
//     const badUUID = '11111111-3333-F444-F888-999999999999'; // Not a valid uuidv4 due to F
//     const unknownBID = '11111111-3333-4444-8888-999999999999'; // valid UUID, but not in blockchains list
//     expect(uuidRegExp.test(badUUID)).toBe(false);
//     expect(uuidRegExp.test(unknownBID)).toBe(true);
//     const badUrls = [
//       ``, `/`, `/.`, `test`, `/test`, `test/dashboard`, `/test/dashboard`,
//       `/${badUUID}`, `/${badUUID}/`, `/${unknownBID}`, `/${unknownBID}/`,
//       `/${badUUID}/unregistered-child-route`,
//       `/${unknownBID}/unregistered-child-route`,
//       `/${badUUID}/unregistered-child-route/multi-level1/multi-level2`,
//       `/${unknownBID}/unregistered-child-route/multi-level1/multi-level2`,
//       `/${mainRoutes.blockchain}/unregistered-route`, // should only allow /blockchain/{deploy|zones|deploying|welcome}
//       `/${mainRoutes.blockchain}/unregistered-route/`, // unregistered-route as child route should be redirected
//       `/#!@#()`, `/----`, `/-.-.-`
//     ];
//     for (const childRoute of mainRoutes.blockchainIdChildren) {
//       // generate more bad urls to check (bad bId, but good childRoute. e.g. /{bad-bId}/dashboard)
//       badUrls.push(`/${badUUID}/${childRoute}`);
//       badUrls.push(`/${unknownBID}/${childRoute}`);
//       badUrls.push(`/${badUUID}/${childRoute}/`);
//       badUrls.push(`/${unknownBID}/${childRoute}/`);
//       badUrls.push(`/${badUUID}/${childRoute}/unregistered-route-123`);
//       badUrls.push(`/${unknownBID}/${childRoute}/unregistered-route-123`);
//     }
//     const redirectFuncSpy = spyOn(routeService, 'redirectToDefault');
//     let callCountBefore = redirectFuncSpy.calls.count();
//     for (const url of badUrls) {
//       test.router.url = url;
//       test.resetComponent();
//       expect(redirectFuncSpy.calls.count()).toBe(callCountBefore + 1); // called once more; bad URL redirected successfully
//       callCountBefore = redirectFuncSpy.calls.count();
//     }
//   });

//   it('good blockchain id present in blockchains list with a valid child route should NOT redirect', () => {
//     const goodURL = `/${mockBlockchains[0].id}/${mainRoutes.dashboard}`;
//     test.router.url = goodURL;
//     const redirectFuncSpy = spyOn(routeService, 'redirectToDefault');
//     const callCountBefore = redirectFuncSpy.calls.count();
//     test.resetComponent();
//     expect(redirectFuncSpy.calls.count()).toEqual(callCountBefore);
//   });

//   it('good blockchain id present in blockchains list WITHOUT a valid child route should redirect', () => {
//     const badURL = `/${mockBlockchains[0].id}/not-valid-child-route`;
//     test.router.url = badURL;
//     const redirectFuncSpy = spyOn(routeService, 'redirectToDefault');
//     const callCountBefore = redirectFuncSpy.calls.count();
//     test.resetComponent();
//     expect(redirectFuncSpy.calls.count()).toEqual(callCountBefore + 1); // redirected.
//   });

//   it('valid non-blockchain paths should not redirect (e.g. /blockchain/deploy)', () => {
//     const goodURLs = [
//       `/${mainRoutes.blockchain}/${mainRoutes.welcome}`,    // /blockchain/welcome
//       // `/${mainRoutes.blockchain}/${mainRoutes.deploy}`,     // /blockchain/deploy
//       `/${mainRoutes.blockchain}/${mainRoutes.deploying}`,  // /blockchain/deploying
//       `/${mainRoutes.blockchain}/${mainRoutes.zones}`,      // /blockchain/zones
//     ];
//     const redirectFuncSpy = spyOn(routeService, 'redirectToDefault');
//     const callCountBefore = redirectFuncSpy.calls.count();
//     for (const url of goodURLs) {
//       test.router.url = url;
//       test.resetComponent();
//       expect(redirectFuncSpy.calls.count()).toEqual(callCountBefore); // not redirected
//     }
//   });

//   it('fragment change to welcome should open the welcome modal', () => {
//     test.resetComponent();
//     const welcomeModalOpen = spyOn(test.component.welcomeModal, 'open');
//     test.activatedRoute.fragment.next(mainFragments.welcome);
//     expect(welcomeModalOpen).toHaveBeenCalledWith();
//   });

//   describe('mock blockchains', () => {
//     it('should provide mock blockchains', () => {
//       // 3 mock blockchains provided (ETH, DAML, ETH-FAILED)
//       blockchainService.provideMockBlockchains();
//       test.refreshComponent();
//       expect(test.component.blockchains.length).toBe(mockBlockchains.length);
//     });
//     it('should provide Swagger-generated blockchains', async () => {
//       await swaggerMocks.sampleResponse('GET /blockchains', 200)
//         .then(blockchains => { blockchainService.blockchains = blockchains; });
//       test.refreshComponent();
//       // Swagger generated GET /blockchains has only 1 example
//       expect(test.component.blockchains.length).toBe(1);
//     });
//   });

// });
