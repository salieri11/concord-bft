webpackJsonp(["main"],{

/***/ "./src/$$_lazy_route_resource lazy recursive":
/***/ (function(module, exports) {

function webpackEmptyAsyncContext(req) {
	// Here Promise.resolve().then() is used instead of new Promise() to prevent
	// uncatched exception popping up in devtools
	return Promise.resolve().then(function() {
		throw new Error("Cannot find module '" + req + "'.");
	});
}
webpackEmptyAsyncContext.keys = function() { return []; };
webpackEmptyAsyncContext.resolve = webpackEmptyAsyncContext;
module.exports = webpackEmptyAsyncContext;
webpackEmptyAsyncContext.id = "./src/$$_lazy_route_resource lazy recursive";

/***/ }),

/***/ "./src/app/app.component.css":
/***/ (function(module, exports) {

module.exports = ".header {\n  background-color: #002538;\n}\n\n.header .branding .nav-link img {\n  width: 100%;\n  max-width: 140px;\n}\n\n.header .dropdown .dropdown-toggle clr-icon[shape='user'] {\n  left: 0.75rem;\n}\n\n.header .dropdown .dropdown-toggle {\n  padding-left: 2.75rem;\n}\n\n.header .header-actions .active-nodes {\n  display: -webkit-box;\n  display: -ms-flexbox;\n  display: flex;\n  -webkit-box-align: center;\n      -ms-flex-align: center;\n          align-items: center;\n}\n\n.header .header-actions .active-nodes clr-icon[shape='circle'] {\n  margin-right: 10px;\n}\n\n.header .header-actions .nav-icon:after {\n  position: absolute;\n  content: \"\";\n  display: inline-block;\n  background: #fafafa;\n  opacity: .15;\n  height: 1.66667rem;\n  width: 1px;\n  top: .41667rem;\n  right: 0;\n}\n\n.header .header-actions .nav-icon:nth-child(2):before {\n  position: absolute;\n  content: \"\";\n  display: inline-block;\n  background: #fafafa;\n  opacity: .15;\n  height: 1.66667rem;\n  width: 1px;\n  top: .41667rem;\n}\n\n.clr-vertical-nav {\n  background-color: #383F43;\n  padding-top: 0;\n}\n\n.clr-vertical-nav .nav-link {\n  color: white;\n  -webkit-box-flex: 0;\n      -ms-flex: 0 0 2.5rem;\n          flex: 0 0 2.5rem;\n  -webkit-box-align: center;\n      -ms-flex-align: center;\n          align-items: center;\n  background-position: 0.75rem 0.7rem;\n  background-repeat: no-repeat;\n}\n\n.clr-vertical-nav .nav-link {\n  color: white;\n  -webkit-box-flex: 0;\n      -ms-flex: 0 0 2.5rem;\n          flex: 0 0 2.5rem;\n  -webkit-box-align: center;\n      -ms-flex-align: center;\n          align-items: center;\n  background-position: 0.75rem 0.7rem;\n  background-repeat: no-repeat;\n}\n\n.clr-vertical-nav .nav-link .nav-icon-large {\n  width: 24px;\n  height: 24px;\n  -webkit-box-flex: 0;\n      -ms-flex: 0 0 24px;\n          flex: 0 0 24px;\n  margin-right: .75rem;\n}\n\n.clr-vertical-nav .nav-link.active, .clr-vertical-nav .nav-link:hover {\n  background-color: #009DDA;\n  border-right: 5px solid #5BD1FF;\n}\n\n.clr-vertical-nav .nav-link.active .nav-icon, .clr-vertical-nav .nav-link:hover .nav-icon {\n  fill: #FFFFFF;\n}\n"

/***/ }),

/***/ "./src/app/app.component.html":
/***/ (function(module, exports) {

module.exports = "<clr-main-container>\n  <clr-header>\n    <div class=\"branding\">\n      <a href=\"#\" class=\"nav-link\">\n        <img src=\"assets/static/images/nav-logo.png\" alt=\"VMware\">\n      </a>\n    </div>\n    <div class=\"header-actions\">\n      <div class=\"nav-text active-nodes\">\n        <clr-icon shape=\"circle\" class=\"is-solid is-success\" size=\"14\"></clr-icon>\n        Active Nodes 28,458\n      </div>\n      <a href=\"javascript:;\" class=\"nav-link nav-icon\">\n        <clr-icon shape=\"search\"></clr-icon>\n      </a>\n      <a href=\"javascript:;\" class=\"nav-link nav-icon\">\n        <clr-icon shape=\"cog\"></clr-icon>\n      </a>\n      <clr-dropdown>\n        <button class=\"nav-text\" clrDropdownTrigger>\n          <clr-icon shape=\"user\" class=\"is-solid\"></clr-icon>\n          john.doe@vmware.com\n          <clr-icon shape=\"caret down\"></clr-icon>\n        </button>\n        <clr-dropdown-menu *clrIfOpen clrPosition=\"bottom-right\">\n          <a href=\"javascript:;\" clrDropdownItem>Log out</a>\n        </clr-dropdown-menu>\n      </clr-dropdown>\n    </div>\n  </clr-header>\n  <div class=\"content-container\">\n    <main class=\"content-area\">\n      <router-outlet></router-outlet>\n    </main>\n    <clr-vertical-nav>\n      <a clrVerticalNavLink routerLink=\"dashboard\" routerLinkActive=\"active\">\n        <clr-icon clrVerticalNavIcon shape=\"home\" class=\"nav-icon-large\"></clr-icon>\n        Dashboard\n      </a>\n      <a clrVerticalNavLink routerLink=\"nodes\" routerLinkActive=\"active\">\n        <clr-icon clrVerticalNavIcon shape=\"network-globe\" class=\"nav-icon-large\"></clr-icon>\n        Node List\n      </a>\n      <a clrVerticalNavLink routerLink=\"blocks\" routerLinkActive=\"active\">\n        <clr-icon clrVerticalNavIcon shape=\"blocks-group\" class=\"nav-icon-large\"></clr-icon>\n        Block List\n      </a>\n    </clr-vertical-nav>\n  </div>\n</clr-main-container>\n"

/***/ }),

/***/ "./src/app/app.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return AppComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};

var AppComponent = /** @class */ (function () {
    function AppComponent() {
        this.title = 'app';
    }
    AppComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-root',
            template: __webpack_require__("./src/app/app.component.html"),
            styles: [__webpack_require__("./src/app/app.component.css")]
        })
    ], AppComponent);
    return AppComponent;
}());



/***/ }),

/***/ "./src/app/app.module.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return AppModule; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_platform_browser__ = __webpack_require__("./node_modules/@angular/platform-browser/esm5/platform-browser.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_platform_browser_animations__ = __webpack_require__("./node_modules/@angular/platform-browser/esm5/animations.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__angular_common_http__ = __webpack_require__("./node_modules/@angular/common/esm5/http.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__angular_router__ = __webpack_require__("./node_modules/@angular/router/esm5/router.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__clr_angular__ = __webpack_require__("./node_modules/@clr/angular/esm5/clr-angular.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__dashboard_dashboard_module__ = __webpack_require__("./src/app/dashboard/dashboard.module.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__nodes_nodes_module__ = __webpack_require__("./src/app/nodes/nodes.module.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__blocks_blocks_module__ = __webpack_require__("./src/app/blocks/blocks.module.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__app_component__ = __webpack_require__("./src/app/app.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};










var appRoutes = [
    { path: '',
        redirectTo: 'dashboard',
        pathMatch: 'full'
    }
];
var AppModule = /** @class */ (function () {
    function AppModule() {
    }
    AppModule = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_2__angular_core__["J" /* NgModule */])({
            declarations: [
                __WEBPACK_IMPORTED_MODULE_9__app_component__["a" /* AppComponent */]
            ],
            imports: [
                __WEBPACK_IMPORTED_MODULE_0__angular_platform_browser__["a" /* BrowserModule */],
                __WEBPACK_IMPORTED_MODULE_1__angular_platform_browser_animations__["a" /* BrowserAnimationsModule */],
                __WEBPACK_IMPORTED_MODULE_3__angular_common_http__["b" /* HttpClientModule */],
                __WEBPACK_IMPORTED_MODULE_5__clr_angular__["a" /* ClarityModule */],
                __WEBPACK_IMPORTED_MODULE_4__angular_router__["b" /* RouterModule */].forRoot(appRoutes, { enableTracing: true }),
                __WEBPACK_IMPORTED_MODULE_6__dashboard_dashboard_module__["a" /* DashboardModule */],
                __WEBPACK_IMPORTED_MODULE_7__nodes_nodes_module__["a" /* NodesModule */],
                __WEBPACK_IMPORTED_MODULE_8__blocks_blocks_module__["a" /* BlocksModule */]
            ],
            providers: [],
            bootstrap: [__WEBPACK_IMPORTED_MODULE_9__app_component__["a" /* AppComponent */]]
        })
    ], AppModule);
    return AppModule;
}());



/***/ }),

/***/ "./src/app/blocks/block-detail-container/block-detail-container.component.css":
/***/ (function(module, exports) {

module.exports = ""

/***/ }),

/***/ "./src/app/blocks/block-detail-container/block-detail-container.component.html":
/***/ (function(module, exports) {

module.exports = "<div class=\"p-2 bg-header\">\n  <div class=\"row\">\n    <div class=\"col-md flex-items-xs-middle media\">\n      <a routerLink=\"/blocks\">\n        <clr-icon shape=\"circle-arrow\" class=\"is-highlight\" size=\"28\" dir=\"left\"></clr-icon>\n      </a>\n      <h2 class=\"mt-0 pl-1\">{{block?.number}}</h2>\n    </div>\n  </div>\n  <dl>\n    <dt>Hash</dt>\n    <dd>{{block?.hash}}</dd>\n    <dt>Nonce</dt>\n    <dd>{{block?.nonce}}</dd>\n    <dt>Parent Hash</dt>\n    <dd>{{block?.parentHash}}</dd>\n    <dt>Size</dt>\n    <dd>{{block?.size}}</dd>\n  </dl>\n</div>\n\n<div class=\"p-2\">\n  <h3 class=\"mt-0\">Transactions</h3>\n  <table class=\"table\">\n    <thead>\n    <tr>\n      <th class=\"text-left\">Hash</th>\n    </tr>\n    </thead>\n    <tbody>\n    <tr *ngFor=\"let transaction of block?.transactions\">\n      <td class=\"text-left\">{{transaction}}</td>\n    </tr>\n    </tbody>\n  </table>\n</div>\n"

/***/ }),

/***/ "./src/app/blocks/block-detail-container/block-detail-container.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return BlockDetailContainerComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common_http__ = __webpack_require__("./node_modules/@angular/common/esm5/http.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__angular_router__ = __webpack_require__("./node_modules/@angular/router/esm5/router.js");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};



/**
 * Displays a single block's details
 */
var BlockDetailContainerComponent = /** @class */ (function () {
    function BlockDetailContainerComponent(httpClient, route) {
        this.httpClient = httpClient;
        this.route = route;
    }
    BlockDetailContainerComponent.prototype.ngOnInit = function () {
        var _this = this;
        this.route.params.subscribe(function (params) {
            _this.loadBlock(params.id);
        });
    };
    BlockDetailContainerComponent.prototype.loadBlock = function (blockNumber) {
        var _this = this;
        this.httpClient.get("/api/athena/blocks/" + blockNumber).subscribe(function (data) { return _this.block = data; });
    };
    BlockDetailContainerComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-block-detail-container',
            template: __webpack_require__("./src/app/blocks/block-detail-container/block-detail-container.component.html"),
            styles: [__webpack_require__("./src/app/blocks/block-detail-container/block-detail-container.component.css")]
        }),
        __metadata("design:paramtypes", [__WEBPACK_IMPORTED_MODULE_1__angular_common_http__["a" /* HttpClient */], __WEBPACK_IMPORTED_MODULE_2__angular_router__["a" /* ActivatedRoute */]])
    ], BlockDetailContainerComponent);
    return BlockDetailContainerComponent;
}());



/***/ }),

/***/ "./src/app/blocks/blocks-container/blocks-container.component.css":
/***/ (function(module, exports) {

module.exports = ""

/***/ }),

/***/ "./src/app/blocks/blocks-container/blocks-container.component.html":
/***/ (function(module, exports) {

module.exports = "<div class=\"p-2 bg-dark\">\n  <div class=\"row\">\n    <div class=\"col-xs-4 media flex-items-xs-middle\">\n      <h3 class=\"mt-0 text-white\">Block Activity</h3>\n    </div>\n    <div class=\"col-xs-8 media flex-items-xs-middle flex-items-xs-right\">\n      <clr-tabs>\n        <clr-tab>\n          <button clrTabLink>Today</button>\n          <clr-tab-content id=\"content1\" *clrIfActive=\"true\"></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 7 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 30 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 90 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n      </clr-tabs>\n    </div>\n  </div>\n</div>\n\n<img class=\"graph-placeholder\" src=\"assets/static/images/node-activity-large.png\">\n\n<div class=\"p-2\">\n  <h3 class=\"mt-0\">Blocks</h3>\n\n  <table class=\"table\">\n    <thead>\n    <tr>\n      <th class=\"text-right\">Index</th>\n      <th class=\"text-left\">Hash</th>\n      <th></th>\n    </tr>\n    </thead>\n    <tbody>\n    <tr *ngFor=\"let block of blocks\">\n      <td class=\"text-right\">{{block.number}}</td>\n      <td class=\"text-left\">{{block.hash}}</td>\n      <td>\n        <clr-dropdown [clrCloseMenuOnItemClick]=\"false\">\n          <button type=\"button\" class=\"px-0\" clrDropdownTrigger>\n            <clr-icon shape=\"ellipsis-horizontal\" size=\"16\"></clr-icon>\n          </button>\n          <clr-dropdown-menu *clrIfOpen clrPosition=\"bottom-right\">\n            <a clrDropdownItem [routerLink]=\"['/blocks', block.number]\">View Detail</a>\n          </clr-dropdown-menu>\n        </clr-dropdown>\n      </td>\n    </tr>\n    </tbody>\n  </table>\n\n  <div class=\"text-right\">\n    <button class=\"btn btn-primary\" [disabled]=\"!nextBlockUrl\" (click)=\"loadNextBlocks()\">Next</button>\n  </div>\n</div>\n"

/***/ }),

/***/ "./src/app/blocks/blocks-container/blocks-container.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return BlocksContainerComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common_http__ = __webpack_require__("./node_modules/@angular/common/esm5/http.js");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};


/**
 * Displays a paginated listing of blocks
 */
var BlocksContainerComponent = /** @class */ (function () {
    function BlocksContainerComponent(httpClient) {
        this.httpClient = httpClient;
        this.blocks = [];
        this.nextBlockUrl = '/api/athena/blocks';
    }
    BlocksContainerComponent.prototype.ngOnInit = function () {
        this.loadNextBlocks();
    };
    BlocksContainerComponent.prototype.loadNextBlocks = function () {
        var _this = this;
        this.httpClient.get(this.nextBlockUrl).subscribe(function (data) {
            _this.blocks = data.blocks;
            // TODO: Abstract to class with hasNext(), next(), etc. Support pagination in URL directly. Need to support prev?
            if (data.next && data.next !== _this.nextBlockUrl) {
                _this.nextBlockUrl = data.next;
            }
            else {
                _this.nextBlockUrl = undefined;
            }
        });
    };
    BlocksContainerComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-blocks-container',
            template: __webpack_require__("./src/app/blocks/blocks-container/blocks-container.component.html"),
            styles: [__webpack_require__("./src/app/blocks/blocks-container/blocks-container.component.css")]
        }),
        __metadata("design:paramtypes", [__WEBPACK_IMPORTED_MODULE_1__angular_common_http__["a" /* HttpClient */]])
    ], BlocksContainerComponent);
    return BlocksContainerComponent;
}());



/***/ }),

/***/ "./src/app/blocks/blocks.module.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return BlocksModule; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common__ = __webpack_require__("./node_modules/@angular/common/esm5/common.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__angular_router__ = __webpack_require__("./node_modules/@angular/router/esm5/router.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__clr_angular__ = __webpack_require__("./node_modules/@clr/angular/esm5/clr-angular.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__blocks_container_blocks_container_component__ = __webpack_require__("./src/app/blocks/blocks-container/blocks-container.component.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__block_detail_container_block_detail_container_component__ = __webpack_require__("./src/app/blocks/block-detail-container/block-detail-container.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};






var routes = [
    { path: 'blocks', component: __WEBPACK_IMPORTED_MODULE_4__blocks_container_blocks_container_component__["a" /* BlocksContainerComponent */] },
    { path: 'blocks/:id', component: __WEBPACK_IMPORTED_MODULE_5__block_detail_container_block_detail_container_component__["a" /* BlockDetailContainerComponent */] }
];
var BlocksModule = /** @class */ (function () {
    function BlocksModule() {
    }
    BlocksModule = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["J" /* NgModule */])({
            imports: [
                __WEBPACK_IMPORTED_MODULE_1__angular_common__["b" /* CommonModule */],
                __WEBPACK_IMPORTED_MODULE_2__angular_router__["b" /* RouterModule */].forChild(routes),
                __WEBPACK_IMPORTED_MODULE_3__clr_angular__["a" /* ClarityModule */]
            ],
            declarations: [__WEBPACK_IMPORTED_MODULE_4__blocks_container_blocks_container_component__["a" /* BlocksContainerComponent */], __WEBPACK_IMPORTED_MODULE_5__block_detail_container_block_detail_container_component__["a" /* BlockDetailContainerComponent */]]
        })
    ], BlocksModule);
    return BlocksModule;
}());



/***/ }),

/***/ "./src/app/dashboard/dashboard-container/dashboard-container.component.css":
/***/ (function(module, exports) {

module.exports = ""

/***/ }),

/***/ "./src/app/dashboard/dashboard-container/dashboard-container.component.html":
/***/ (function(module, exports) {

module.exports = "<img class=\"graph-placeholder\" src=\"assets/static/images/node-location-large.png\">\n\n<div class=\"p-2 bg-header\">\n  <div class=\"row\">\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Total Active Nodes\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.totalActiveNodes | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Inactive Nodes\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.inactiveNodes | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Overall Node Health\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.overallNodeHealth | percent:\"1.0-2\"}}</h2>\n      <div class=\"progress success\">\n        <progress max=\"100\" [value]=\"mockStats.overallNodeHealth * 100\"></progress>\n      </div>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Transactions/second\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.transactionsPerSecond | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Avg Validation Time\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.averageValidationTime | number:\"1.0-2\"}} <small>seconds</small></h2>\n    </div>\n  </div>\n</div>\n\n<div class=\"p-2\">\n  <div class=\"row flex-items-xs-middle\">\n    <div class=\"col-sm\">\n      <h3 class=\"mt-0\">Recent Transactions</h3>\n    </div>\n  </div>\n\n  <table class=\"table\">\n    <thead>\n    <tr>\n      <th>Timestamp</th>\n      <th>Origination</th>\n      <th>Transaction Type</th>\n      <th>Amount</th>\n      <th>Trans Length</th>\n      <th>Status</th>\n      <th>Nodes</th>\n      <th></th>\n    </tr>\n    </thead>\n    <tbody>\n    <tr>\n      <td>01-28-2017</td>\n      <td>Lindberg</td>\n      <td>Lorem ipsum dolor</td>\n      <td>$785</td>\n      <td>40.87 sec</td>\n      <td>Denied</td>\n      <td>75</td>\n      <td>\n        <clr-dropdown [clrCloseMenuOnItemClick]=\"false\">\n          <button type=\"button\" clrDropdownTrigger>\n            <clr-icon shape=\"ellipsis-horizontal\" size=\"16\"></clr-icon>\n          </button>\n          <clr-dropdown-menu *clrIfOpen clrPosition=\"bottom-right\">\n            <button type=\"button\" clrDropdownItem>Lorem ipsum</button>\n            <button type=\"button\" clrDropdownItem>Dolor sit amet</button>\n          </clr-dropdown-menu>\n        </clr-dropdown>\n      </td>\n    </tr>\n    </tbody>\n  </table>\n</div>\n"

/***/ }),

/***/ "./src/app/dashboard/dashboard-container/dashboard-container.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return DashboardContainerComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};

var DashboardContainerComponent = /** @class */ (function () {
    function DashboardContainerComponent() {
        this.mockStats = {
            totalActiveNodes: 28458,
            inactiveNodes: 583,
            overallNodeHealth: .8742123,
            transactionsPerSecond: 4289,
            averageValidationTime: 1.98
        };
    }
    DashboardContainerComponent.prototype.ngOnInit = function () {
    };
    DashboardContainerComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-dashboard-container',
            template: __webpack_require__("./src/app/dashboard/dashboard-container/dashboard-container.component.html"),
            styles: [__webpack_require__("./src/app/dashboard/dashboard-container/dashboard-container.component.css")]
        }),
        __metadata("design:paramtypes", [])
    ], DashboardContainerComponent);
    return DashboardContainerComponent;
}());



/***/ }),

/***/ "./src/app/dashboard/dashboard.module.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return DashboardModule; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common__ = __webpack_require__("./node_modules/@angular/common/esm5/common.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__clr_angular__ = __webpack_require__("./node_modules/@clr/angular/esm5/clr-angular.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__angular_router__ = __webpack_require__("./node_modules/@angular/router/esm5/router.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__dashboard_container_dashboard_container_component__ = __webpack_require__("./src/app/dashboard/dashboard-container/dashboard-container.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};





var routes = [
    { path: 'dashboard', component: __WEBPACK_IMPORTED_MODULE_4__dashboard_container_dashboard_container_component__["a" /* DashboardContainerComponent */] }
];
var DashboardModule = /** @class */ (function () {
    function DashboardModule() {
    }
    DashboardModule = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["J" /* NgModule */])({
            imports: [
                __WEBPACK_IMPORTED_MODULE_1__angular_common__["b" /* CommonModule */],
                __WEBPACK_IMPORTED_MODULE_3__angular_router__["b" /* RouterModule */].forChild(routes),
                __WEBPACK_IMPORTED_MODULE_2__clr_angular__["a" /* ClarityModule */]
            ],
            declarations: [__WEBPACK_IMPORTED_MODULE_4__dashboard_container_dashboard_container_component__["a" /* DashboardContainerComponent */]]
        })
    ], DashboardModule);
    return DashboardModule;
}());



/***/ }),

/***/ "./src/app/nodes/node-detail-container/node-detail-container.component.css":
/***/ (function(module, exports) {

module.exports = "p.detail-sub-stat {\n  margin-top: 0.25rem;\n}\n"

/***/ }),

/***/ "./src/app/nodes/node-detail-container/node-detail-container.component.html":
/***/ (function(module, exports) {

module.exports = "<div class=\"p-2 bg-header\">\n  <div class=\"row\">\n    <div class=\"col-md flex-items-xs-middle media\">\n      <a routerLink=\"/nodes\">\n        <clr-icon shape=\"circle-arrow\" class=\"is-highlight\" size=\"28\" dir=\"left\"></clr-icon>\n      </a>\n      <h2 class=\"mt-0 pl-1\">GAV-324V AAC Application Node</h2>\n    </div>\n  </div>\n  <div class=\"row\">\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Location\n      </p>\n      <p class=\"mt-0\">New York, NY</p>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Node Owners\n      </p>\n      <p class=\"mt-0\">Allen, J</p>\n      <p class=\"mt-0\">Dickerson, B</p>\n      <p class=\"mt-0\">McCready, M</p>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Overall Node Health\n      </p>\n      <h2 class=\"mt-0\">74%</h2>\n      <div class=\"progress warning\">\n        <progress max=\"100\" [value]=\"74\"></progress>\n      </div>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Transactions/Second\n      </p>\n      <h2 class=\"mt-0\">189</h2>\n      <div class=\"progress-with-datapoint\">\n        <div class=\"progress black\">\n          <progress max=\"100\" [value]=\"50\"></progress>\n        </div>\n        <div class=\"progress-data-point danger\"></div>\n      </div>\n      <p class=\"p7 detail-sub-stat text-center\">\n        Avg 210/sec\n      </p>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Avg Validation Time\n      </p>\n      <h2 class=\"mt-0\">2.28 <small>seconds</small></h2>\n      <div class=\"progress-with-datapoint\">\n        <div class=\"progress black\">\n          <progress max=\"100\" [value]=\"50\"></progress>\n        </div>\n        <div class=\"progress-data-point success\"></div>\n      </div>\n      <p class=\"p7 detail-sub-stat text-center\">\n        Avg 2.58 sec\n      </p>\n    </div>\n  </div>\n  <div class=\"row\">\n    <div class=\"col-md-12\">\n      <h3 class=\"mb-1\">Network</h3>\n      <img class=\"graph-placeholder\" src=\"assets/static/images/network-timeline.png\">\n    </div>\n    <div class=\"col-md-6\">\n      <h3 class=\"mb-1\">Node Activity</h3>\n      <img class=\"graph-placeholder\" src=\"assets/static/images/node-activity-small.png\">\n    </div>\n    <div class=\"col-md-6\">\n      <h3 class=\"mb-1\">Node Location</h3>\n      <img class=\"graph-placeholder\" src=\"assets/static/images/node-location-small.png\">\n    </div>\n  </div>\n</div>\n\n<div class=\"p-2\">\n  <div class=\"row\">\n    <div class=\"col-xs-6\">\n      <h3 class=\"mt-0\">Transactions</h3>\n    </div>\n    <div class=\"col-xs-6\">\n      <div class=\"form-group text-right\">\n        <input type=\"text\" placeholder=\"Search...\" />\n        <button type=\"button\" class=\"btn btn-primary\" (click)=\"onOpenFilterModal()\">Filters</button>\n      </div>\n    </div>\n  </div>\n\n  <table class=\"table\">\n    <thead>\n    <tr>\n      <th>Timestamp</th>\n      <th>Origination</th>\n      <th>Transaction Type</th>\n      <th>Amount</th>\n      <th>Trans Length</th>\n      <th>Status</th>\n      <th>Nodes</th>\n      <th></th>\n    </tr>\n    </thead>\n    <tbody>\n    <tr>\n      <td>01-28-2017</td>\n      <td>Lindberg</td>\n      <td>Lorem ipsum dolor</td>\n      <td>$785</td>\n      <td>40.87 sec</td>\n      <td>Denied</td>\n      <td>75</td>\n      <td>\n        <clr-dropdown [clrCloseMenuOnItemClick]=\"false\">\n          <button type=\"button\" clrDropdownTrigger>\n            <clr-icon shape=\"ellipsis-horizontal\" size=\"16\"></clr-icon>\n          </button>\n          <clr-dropdown-menu *clrIfOpen clrPosition=\"bottom-right\">\n            <button type=\"button\" clrDropdownItem>Lorem ipsum</button>\n            <button type=\"button\" clrDropdownItem>Dolor sit amet</button>\n          </clr-dropdown-menu>\n        </clr-dropdown>\n      </td>\n    </tr>\n    </tbody>\n  </table>\n</div>\n\n<app-transaction-filters-modal #filterModal (applyFilters)=\"onApplyFilters()\"></app-transaction-filters-modal>\n"

/***/ }),

/***/ "./src/app/nodes/node-detail-container/node-detail-container.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return NodeDetailContainerComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__transaction_filters_modal_transaction_filters_modal_component__ = __webpack_require__("./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};


var NodeDetailContainerComponent = /** @class */ (function () {
    function NodeDetailContainerComponent() {
    }
    NodeDetailContainerComponent.prototype.ngOnInit = function () {
    };
    NodeDetailContainerComponent.prototype.onOpenFilterModal = function () {
        this.filterModal.open();
    };
    NodeDetailContainerComponent.prototype.onApplyFilters = function () {
        // TODO: action on apply filters
    };
    __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["_10" /* ViewChild */])('filterModal'),
        __metadata("design:type", __WEBPACK_IMPORTED_MODULE_1__transaction_filters_modal_transaction_filters_modal_component__["a" /* TransactionFiltersModalComponent */])
    ], NodeDetailContainerComponent.prototype, "filterModal", void 0);
    NodeDetailContainerComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-node-detail-container',
            template: __webpack_require__("./src/app/nodes/node-detail-container/node-detail-container.component.html"),
            styles: [__webpack_require__("./src/app/nodes/node-detail-container/node-detail-container.component.css")]
        }),
        __metadata("design:paramtypes", [])
    ], NodeDetailContainerComponent);
    return NodeDetailContainerComponent;
}());



/***/ }),

/***/ "./src/app/nodes/nodes-container/nodes-container.component.css":
/***/ (function(module, exports) {

module.exports = ""

/***/ }),

/***/ "./src/app/nodes/nodes-container/nodes-container.component.html":
/***/ (function(module, exports) {

module.exports = "<div class=\"p-2 bg-dark\">\n  <div class=\"row\">\n    <div class=\"col-xs-4 media flex-items-xs-middle\">\n      <h3 class=\"mt-0 text-white\">Node Activity</h3>\n    </div>\n    <div class=\"col-xs-8 media flex-items-xs-middle flex-items-xs-right\">\n      <clr-tabs>\n        <clr-tab>\n          <button clrTabLink>Today</button>\n          <clr-tab-content id=\"content1\" *clrIfActive=\"true\"></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 7 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 30 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n        <clr-tab>\n          <button clrTabLink>Last 90 Days</button>\n          <clr-tab-content *clrIfActive></clr-tab-content>\n        </clr-tab>\n      </clr-tabs>\n    </div>\n  </div>\n</div>\n\n<img class=\"graph-placeholder\" src=\"assets/static/images/node-activity-large.png\">\n\n<div class=\"p-2 bg-header\">\n  <div class=\"row\">\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Total Active Nodes\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.totalActiveNodes | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\" >\n        Inactive Nodes\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.inactiveNodes | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Overall Node Health\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.overallNodeHealth | percent:\"1.0-2\"}}</h2>\n      <div class=\"progress success\">\n        <progress max=\"100\" [value]=\"mockStats.overallNodeHealth * 100\"></progress>\n      </div>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Transactions/second\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.transactionsPerSecond | number}}</h2>\n    </div>\n    <div class=\"col-md\">\n      <p class=\"p7 mt-0\">\n        Avg Validation Time\n      </p>\n      <h2 class=\"mt-0\">{{mockStats.averageValidationTime | number:\"1.0-2\"}} <small>seconds</small></h2>\n    </div>\n  </div>\n</div>\n\n<div class=\"p-2\">\n  <div class=\"row\">\n    <div class=\"col-xs-6\">\n      <h3 class=\"mt-0\">Nodes</h3>\n    </div>\n    <div class=\"col-xs-6\">\n      <div class=\"form-group text-right\">\n        <input type=\"text\" placeholder=\"Search...\" />\n        <button type=\"button\" class=\"btn btn-primary\" (click)=\"onOpenFilterModal()\">Filters</button>\n      </div>\n    </div>\n  </div>\n\n  <table class=\"table\">\n    <thead>\n    <tr>\n      <th>Location</th>\n      <th>Hostname</th>\n      <th>Address</th>\n      <th>Status</th>\n      <th></th>\n    </tr>\n    </thead>\n    <tbody>\n    <tr *ngFor=\"let member of members\">\n      <td>Lindberg</td>\n      <td>{{member.host}}</td>\n      <td>2001:0db8:85a3:0000:0000:8a2e:0370:7334:8080</td>\n      <td>{{member.status}}</td>\n      <td>\n        <clr-dropdown [clrCloseMenuOnItemClick]=\"false\">\n          <button type=\"button\" clrDropdownTrigger>\n            <clr-icon shape=\"ellipsis-horizontal\" size=\"16\"></clr-icon>\n          </button>\n          <clr-dropdown-menu *clrIfOpen clrPosition=\"bottom-right\">\n            <a clrDropdownItem [routerLink]=\"['/nodes', member.host]\">View Detail</a>\n          </clr-dropdown-menu>\n        </clr-dropdown>\n      </td>\n    </tr>\n    </tbody>\n  </table>\n</div>\n\n<app-transaction-filters-modal #filterModal (applyFilters)=\"onApplyFilters()\"></app-transaction-filters-modal>\n"

/***/ }),

/***/ "./src/app/nodes/nodes-container/nodes-container.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return NodesContainerComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common_http__ = __webpack_require__("./node_modules/@angular/common/esm5/http.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__transaction_filters_modal_transaction_filters_modal_component__ = __webpack_require__("./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};



var NodesContainerComponent = /** @class */ (function () {
    function NodesContainerComponent(httpClient) {
        this.httpClient = httpClient;
        this.mockStats = {
            totalActiveNodes: 28458,
            inactiveNodes: 583,
            overallNodeHealth: .8742123,
            transactionsPerSecond: 4289,
            averageValidationTime: 1.98
        };
        this.members = [];
    }
    NodesContainerComponent.prototype.ngOnInit = function () {
        this.loadMembers();
    };
    NodesContainerComponent.prototype.loadMembers = function () {
        var _this = this;
        this.httpClient.get('/api/athena/members').subscribe(function (data) { return _this.members = data; });
    };
    NodesContainerComponent.prototype.onOpenFilterModal = function () {
        this.filterModal.open();
    };
    NodesContainerComponent.prototype.onApplyFilters = function () {
        // TODO: action on apply filters
    };
    __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["_10" /* ViewChild */])('filterModal'),
        __metadata("design:type", __WEBPACK_IMPORTED_MODULE_2__transaction_filters_modal_transaction_filters_modal_component__["a" /* TransactionFiltersModalComponent */])
    ], NodesContainerComponent.prototype, "filterModal", void 0);
    NodesContainerComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-nodes-container',
            template: __webpack_require__("./src/app/nodes/nodes-container/nodes-container.component.html"),
            styles: [__webpack_require__("./src/app/nodes/nodes-container/nodes-container.component.css")]
        }),
        __metadata("design:paramtypes", [__WEBPACK_IMPORTED_MODULE_1__angular_common_http__["a" /* HttpClient */]])
    ], NodesContainerComponent);
    return NodesContainerComponent;
}());



/***/ }),

/***/ "./src/app/nodes/nodes.module.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return NodesModule; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_common__ = __webpack_require__("./node_modules/@angular/common/esm5/common.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__angular_router__ = __webpack_require__("./node_modules/@angular/router/esm5/router.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__clr_angular__ = __webpack_require__("./node_modules/@clr/angular/esm5/clr-angular.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__nodes_container_nodes_container_component__ = __webpack_require__("./src/app/nodes/nodes-container/nodes-container.component.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__node_detail_container_node_detail_container_component__ = __webpack_require__("./src/app/nodes/node-detail-container/node-detail-container.component.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__transaction_filters_modal_transaction_filters_modal_component__ = __webpack_require__("./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};







var routes = [
    { path: 'nodes', component: __WEBPACK_IMPORTED_MODULE_4__nodes_container_nodes_container_component__["a" /* NodesContainerComponent */] },
    { path: 'nodes/:id', component: __WEBPACK_IMPORTED_MODULE_5__node_detail_container_node_detail_container_component__["a" /* NodeDetailContainerComponent */] }
];
var NodesModule = /** @class */ (function () {
    function NodesModule() {
    }
    NodesModule = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["J" /* NgModule */])({
            imports: [
                __WEBPACK_IMPORTED_MODULE_1__angular_common__["b" /* CommonModule */],
                __WEBPACK_IMPORTED_MODULE_2__angular_router__["b" /* RouterModule */].forChild(routes),
                __WEBPACK_IMPORTED_MODULE_3__clr_angular__["a" /* ClarityModule */]
            ],
            declarations: [__WEBPACK_IMPORTED_MODULE_4__nodes_container_nodes_container_component__["a" /* NodesContainerComponent */], __WEBPACK_IMPORTED_MODULE_5__node_detail_container_node_detail_container_component__["a" /* NodeDetailContainerComponent */], __WEBPACK_IMPORTED_MODULE_6__transaction_filters_modal_transaction_filters_modal_component__["a" /* TransactionFiltersModalComponent */]]
        })
    ], NodesModule);
    return NodesModule;
}());



/***/ }),

/***/ "./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.css":
/***/ (function(module, exports) {

module.exports = ""

/***/ }),

/***/ "./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.html":
/***/ (function(module, exports) {

module.exports = "<clr-modal [(clrModalOpen)]=\"isOpened\">\n  <h3 class=\"modal-title\">Filter Transactions</h3>\n  <div class=\"modal-body\">\n    <form>\n      <div class=\"form-block\">\n        <div class=\"form-group\">\n          <label for=\"transaction-status\">By Status</label>\n          <div class=\"select\">\n            <select name=\"transaction-status\" id=\"transaction-status\">\n              <option value=\"\">Pending Transactions</option>\n            </select>\n          </div>\n        </div>\n        <div class=\"form-group\">\n          <label for=\"transaction-type\">By Transaction Type</label>\n          <div class=\"select\">\n            <select name=\"transaction-type\" id=\"transaction-type\">\n              <option value=\"\">All Transactions</option>\n            </select>\n          </div>\n        </div>\n        <div class=\"form-group\">\n          <label for=\"transaction-date-range\">By Date Range</label>\n          <div class=\"select\">\n            <select name=\"transaction-date-range\" id=\"transaction-date-range\">\n              <option value=\"\">Past 7 Days</option>\n            </select>\n          </div>\n        </div>\n        <div class=\"form-group\">\n          <label for=\"transaction-owner\">By Owner</label>\n          <div class=\"select\">\n            <select name=\"transaction-owner\" id=\"transaction-owner\">\n              <option value=\"\">All Owners</option>\n            </select>\n          </div>\n        </div>\n      </div>\n    </form>\n  </div>\n  <div class=\"modal-footer\">\n    <button type=\"button\" class=\"btn btn-outline\" (click)=\"onCancel()\">Cancel</button>\n    <button type=\"button\" class=\"btn btn-primary\" (click)=\"onApply()\">Apply</button>\n  </div>\n</clr-modal>\n"

/***/ }),

/***/ "./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return TransactionFiltersModalComponent; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/*
 * Copyright 2018 VMware, all rights reserved.
 */
var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};

var TransactionFiltersModalComponent = /** @class */ (function () {
    function TransactionFiltersModalComponent() {
        this.applyFilters = new __WEBPACK_IMPORTED_MODULE_0__angular_core__["w" /* EventEmitter */]();
        this.isOpened = false;
    }
    TransactionFiltersModalComponent.prototype.ngOnInit = function () {
    };
    TransactionFiltersModalComponent.prototype.open = function () {
        this.isOpened = true;
    };
    TransactionFiltersModalComponent.prototype.onCancel = function () {
        this.isOpened = false;
    };
    TransactionFiltersModalComponent.prototype.onApply = function () {
        this.applyFilters.emit();
        this.isOpened = false;
    };
    __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["Q" /* Output */])(),
        __metadata("design:type", __WEBPACK_IMPORTED_MODULE_0__angular_core__["w" /* EventEmitter */])
    ], TransactionFiltersModalComponent.prototype, "applyFilters", void 0);
    TransactionFiltersModalComponent = __decorate([
        Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["n" /* Component */])({
            selector: 'app-transaction-filters-modal',
            template: __webpack_require__("./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.html"),
            styles: [__webpack_require__("./src/app/nodes/transaction-filters-modal/transaction-filters-modal.component.css")]
        }),
        __metadata("design:paramtypes", [])
    ], TransactionFiltersModalComponent);
    return TransactionFiltersModalComponent;
}());



/***/ }),

/***/ "./src/environments/environment.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return environment; });
/*
 * Copyright 2018 VMware, all rights reserved.
 */
// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.
var environment = {
    production: false
};


/***/ }),

/***/ "./src/main.ts":
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__angular_core__ = __webpack_require__("./node_modules/@angular/core/esm5/core.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__angular_platform_browser_dynamic__ = __webpack_require__("./node_modules/@angular/platform-browser-dynamic/esm5/platform-browser-dynamic.js");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__app_app_module__ = __webpack_require__("./src/app/app.module.ts");
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__environments_environment__ = __webpack_require__("./src/environments/environment.ts");
/*
 * Copyright 2018 VMware, all rights reserved.
 */




if (__WEBPACK_IMPORTED_MODULE_3__environments_environment__["a" /* environment */].production) {
    Object(__WEBPACK_IMPORTED_MODULE_0__angular_core__["_15" /* enableProdMode */])();
}
Object(__WEBPACK_IMPORTED_MODULE_1__angular_platform_browser_dynamic__["a" /* platformBrowserDynamic */])().bootstrapModule(__WEBPACK_IMPORTED_MODULE_2__app_app_module__["a" /* AppModule */])
    .catch(function (err) { return console.log(err); });


/***/ }),

/***/ 0:
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__("./src/main.ts");


/***/ })

},[0]);
//# sourceMappingURL=main.bundle.js.map