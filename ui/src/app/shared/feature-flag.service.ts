/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Injectable, TemplateRef } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Subject, Observable, of } from 'rxjs';
import { map, catchError } from 'rxjs/operators';
import { environment } from '../../environments/environment';

/**
 * Most of the time flag condition compute requests will come from directives,
 * This helps with which Angular component template is throwing error and under which element tag
 */
export interface FeatureFlagDirectiveSource { componentName?: string; parentTagName?: string; }

/** Event object supplied to subscribers when flags change in value */
export interface FeatureFlagUpdateEvent { name: string; value: boolean; oldValue: boolean; }

@Injectable({
  providedIn: 'root'
})
export class FeatureFlagService {

  public static suppressWarningLogs: boolean = false;
  public static suppressErrorLogs: boolean = false;

  public static truthy: any[] = [true];
  public static falsey: any[] = [false];

  /** Since template flags expressions in directives don't change after load,
   * there is really no need to compute the expression again and again.
   * */
  public cacheExpressionResults: boolean = true;
  public featureFlagsFromConfig: {} = null; // UI dev only overrides
  public featureFlagsFromHelen: {} = null; // Always fetched from Helen after/with auth response

  public readonly featureFlagsChange: Subject<FeatureFlagUpdateEvent> = new Subject<FeatureFlagUpdateEvent>();

  private readonly env = environment;
  private updateSources: (string | Error)[] = []; // Keep track of where all feature flags updates are made
  private featureFlags: {} = {}; // Key-value map from JSON file

  private flagsCache: Map<string, {value: boolean}> = new Map<string, {value: boolean}>();
  private computeResultCache: Map<string, {value: boolean}> = new Map<string, {value: boolean}>();

  constructor(private http: HttpClient) {}

  /** Copy of all current flags data */
  get allFlags(): object { return JSON.parse(JSON.stringify(this.featureFlags)); }

  /** All current flag names in an array */
  get allFlagNames(): [] {
    const flattenKeys = (obj, prefix = '') => Object.keys(obj).reduce((res, el) => {
      if (Array.isArray(obj[el]) ) {
        return res;
      } else if (typeof obj[el] === 'object' && obj[el] !== null ) {
        return res.concat(flattenKeys(obj[el], prefix + el + '.'));
      } else {
        return res.concat([prefix + el]);
      }
    }, []);
    return flattenKeys(this.allFlags);
  }

  /**
   * Is the flag with this flagName on right now?
   * (returns null when not found)
   */
  getFlag(flagName: string): boolean { return this.flagFindByName(flagName); }

  /**
   * Initialize by fetching feature flags file from local or web source.
   *
   * @param sourceURL
   * string value of the flags file target location
   */
  initializeFromURL(sourceURL: string): Observable<{}> {
    return this.http.get(sourceURL).pipe(
        catchError((caught) => {
        let error;
        if (caught instanceof SyntaxError) {
          // Go fix JSON if you see this, JSON parse error
          error = new Error('Feature flags file is not a valid JSON: ' + caught.message);
          console.error(error); console.error(caught);
          console.warn('Falling back to default feature flags...');
        } else {
          /**
           * Most likely HTTP GET error, anything other than 200.
           * make sure app-config.json has set `featureFlagsSource`
           * to a valid fetchable file.
           */
          error = new Error('Cannot fetch feature flags JSON: ' + caught.message);
          console.error(error); console.error(caught);
        }
        return of({ // ? Some default flags when Helen fetch fails?

        });
      }),

      map((flags) => {
        this.initialize(flags, sourceURL);
        return {};
      })

    );
  }

  /**
   * If you already have feature flags data in JSON Object \
   * you can skip fetching and simply initialize with that data.
   */
  initializeWithData(flagsKeyValueMap: object, importSourceInfo: string): void {
    this.initialize(flagsKeyValueMap, importSourceInfo);
  }


  /** Outputs list of all flags updates in the order of update. */
  outputUpdates(): void { let i = 0; for (const src of this.updateSources) { console.log(i, src); ++i; } }


  /**
   * Setting a flag by flag name and value \
   * Flag name can dot-concat to denote nested. (e.g. daml.test.flag1) \
   */
  setFlag(flagName: string, flagValue: any): void {
    try {
      const boolValue = this.flagValueMeansEnabled(flagValue) ? true : false;
      const info = this.flagPathTravelSet(this.featureFlags, flagName, boolValue);
      if (info.value !== info.oldValue) {
        this.flagsCache.set(flagName, {value: boolValue});
        this.computeResultCache.clear();
        this.featureFlagsChange.next({name: flagName, value: info.value, oldValue: info.oldValue});
      }
    } catch (e) {
      if (!FeatureFlagService.suppressErrorLogs) {
        console.error(e);
      }
    }
  }

  /**
   * Usually called to initialize from `importFromSource`. \
   * You can also runtime-override any flags with this function.
   *
   * @param Flags
   * key-value object (supports nested)
   * ```typescript
   * { flag1:true, nested:{flag2:true} }
   * ```
   * ---
   * @param updateSource
   * (optional) string path URL of the source. \
   * If unsupplied, it will add stack trace of the call location
   *
   */
  updateFlags(flagsKeyValueMap: object, updateSource: string = null) {
    if (updateSource !== undefined) { // From known source
      this.updateSources.push(updateSource);
    } else { // Update execute somewhere in Angular codebase
      /** For debug purpose, still keep track of where `updateFlags` has been called;
       * This will nicely help with checking how flags are overriden dynamically in what order. */
      this.updateSources.push(new Error);
    }
    this.flagsRecursiveImport(flagsKeyValueMap, this.featureFlags);
    this.computeResultCache.clear(); // Purge cache
  }

  /**
   * Checks flags expression, possibly from a directive \
   * This ultimately determines whether an element is shown or not.
   *
   * Sadly, 'any' is used below because Angular directives don't care about types; \
   * it will let you pass whatever JS expression in the template.
   */
  checkFlagsCondition(flags: any | any[], templateRef: TemplateRef<any> = null): boolean {

    if (flags === true) { return true; }
    if (flags === false) { return false; }

    // templateReference where the directive call resides, used for debugging
    let sourceComponent: FeatureFlagDirectiveSource = null;
    if (templateRef) {
      try {
        sourceComponent = {
          componentName : templateRef['_parentView'].component.constructor.name,
          parentTagName : templateRef.elementRef.nativeElement.parentElement.localName,
        };
      } catch (e) { console.error(e); }
    }

    // Make into an array
    const wasArray = Array.isArray(flags); if (!wasArray) { flags = [flags]; }

    // Force cast into, specifically, a string array
    flags.forEach(function(flag, i) { if (typeof flag !== 'string') { flags[i] += ''; } });

    // Detect COMPUTE flag
    const overrideToUseCompute = (flags[0] && flags[0] === 'COMPUTE'); if (overrideToUseCompute) { flags.shift(); }
    if (flags.length === 0) {
      if (!FeatureFlagService.suppressWarningLogs) {
        console.warn(new Error(`FeatureFlagService: warning: nothing to check will return true`));
      }
      return true;
    }

    // SIMPLE case with { feauture1 AND feauture2 AND feauture3 ... }
    if (!overrideToUseCompute) {

      for (const flagName of flags) {
        const flagValue = this.flagFindByName(flagName);
        if (flagValue !== null) {
          if (flagValue === false) { return false; }
        } else {
          if (!FeatureFlagService.suppressErrorLogs) {
            if (!sourceComponent) {
              console.error(new Error(`FeatureFlagService: unrecognized feature flag '${flagName}'`));
            } else {
              console.error(new Error(`FeatureFlagService: unrecognized feature flag '${flagName}'`
                + ` in template '${sourceComponent.componentName}'`
                + ` in a child element of '${sourceComponent.parentTagName}'`));
            }
          }
          return false;
        }
      }
      return true;

    // COMPUTE case, feature flags with complex conditions
    } else {

      const flagsExpression: string = flags.join(' and ');
      if (this.cacheExpressionResults) {
        const cached = this.computeResultCache.get(flagsExpression); if (cached) { return cached.value; }
      }

      const computeSafeLogicalExpression = this.getSafeLogicalExpression(flagsExpression, sourceComponent);
      if (computeSafeLogicalExpression === null) { return false; }

      let result: boolean; result = false;
      try {
        // tslint:disable:no-eval
        eval(`result = (${computeSafeLogicalExpression});`);
        if (this.cacheExpressionResults) { this.computeResultCache.set(flagsExpression, {value: result}); }
      } catch (e) {
        if (!FeatureFlagService.suppressErrorLogs) {
          if (e instanceof SyntaxError) {
            console.error(new Error(`Malformed eval strings: '${computeSafeLogicalExpression}'`));
          } else {
            console.error(new Error(`FeatureFlagService: cannot compute: '${flagsExpression}' :: ${e.message}`));
          }
        }
        return false;
      }

      return result;
    }
  }

  /**
   * Parses feature flags expression and outputs a logical expression, \
   * whose words are comprised of [ `&&`, `||`, `!`, `(`, `)`, `true`, `false` ] \
   * If eval string is made up of strictly these words, you can guarantee safety.
   */
  getSafeLogicalExpression(flagsExpression: string, sourceComponent?: FeatureFlagDirectiveSource): string {

    flagsExpression = flagsExpression.replace(/!/g, ' ! ').replace(/\(/g, ' ( ').replace(/\)/g, ' ) ')
                                    .replace(/\s\s+/g, ' ').trim();

    const expressionWords = flagsExpression.split(' ');
    expressionWords.forEach((word, i) => {
      switch (word) {
        case '': case '(': case ')': case '!': case '&&': case '||': case 'true': case 'false': return;
      }
      if (word.length <= 3) {
        const lcase = word.toLowerCase();
        switch (lcase) {
          case 'and' : expressionWords[i] = '&&'; return;
          case 'or' : expressionWords[i] = '||'; return;
          case 'not' : expressionWords[i] = '!'; return;
        }
      }
      const flagValue = this.flagFindByName(word);
      if (flagValue !== null) {
        expressionWords[i] = flagValue ? 'true' : 'false';
      } else {
        if (!FeatureFlagService.suppressErrorLogs) {
          if (!sourceComponent) {
            throw new Error(`FeatureFlagService: unrecognized feature flag '${word}'`);
          } else {
            throw new Error(`FeatureFlagService: unrecognized feature flag '${word}'`
              + ` in template '${sourceComponent.componentName}'`
              + ` in a child element of '${sourceComponent.parentTagName}'`);
          }
        }
        return null;
      }
    });
    return expressionWords.join(' ');

  }

  /** get flag value with given name (e.g. 'some.feature') */
  private flagFindByName(flagName: string): boolean {
    const cached = this.flagsCache.get(flagName); if (cached) { return cached.value; }
    const result = this.flagPathTravel(this.featureFlags, flagName);
    this.flagsCache.set(flagName, {value: result});
    return result;
  }

  /** Resolve dot separated name scheme in flags (e.g. 'daml.contracts.flag1') */
  private flagPathTravel(obj: object, path: string): boolean {
    const keys = path.split('.');
    let target = obj;
    for (const key of keys) {
      if (target[key] === undefined) { return null; }
      target = target[key];
    }
    return target ? true : false;
  }

  /** Resolve dot separated name scheme in flags and set value at that path */
  private flagPathTravelSet(obj: object, path: string, setTo: boolean): FeatureFlagUpdateEvent {
    const keys = path.split('.');
    let target = obj; const lastIndex = keys.length - 1;
    // travel until last object (second to last key), create new path as doing so
    for (let i = 0; i < lastIndex; ++i) {
      const key = keys[i]; if (target[key] === undefined) { target[key] = {}; }
      target = target[key];
    }
    const lastKey = keys[lastIndex];
    const oldValue = target[lastKey]; // return old value, too, for emitting flag value change event
    target[lastKey] = setTo; // set at last travel key
    return {name: path, value: setTo, oldValue: oldValue};
  }

  private flagsRecursiveImport(keyValueMap: object, target: object, path: string = ''): void {
    for (const key of Object.keys(keyValueMap)) {
      const pathNow = (path === '') ? key : path + '.' + key;
      const value = keyValueMap[key];
      if (key.indexOf('.') >= 0) { this.setFlag(pathNow, value); continue; }
      if (Array.isArray(value)) { // array type
        if (!FeatureFlagService.suppressWarningLogs && !this.flagValueIsAmongRecommended(value)) {
            console.warn(new Error(`FeatureFlagService: ${pathNow} is set to unsupported type 'array', ignoring.`));
        }
      } else if (value && typeof value === 'object') { // Object; do recursive import for nested
        if (!target[key]) { target[key] = {}; }
        this.flagsRecursiveImport(value, target[key], pathNow);
      } else { // Primitives
        const oldValue = target[key];
        target[key] = value;
        if (oldValue !== value) { // emit flag change
          this.flagsCache.set(pathNow, {value: value});
          this.featureFlagsChange.next({name: pathNow, value: value, oldValue: oldValue});
        }
        if (!FeatureFlagService.suppressWarningLogs && !this.flagValueIsAmongRecommended(value)) {
            console.warn(new Error(`FeatureFlagService: ${pathNow} not using recommended value (got '${value}')`));
        }
      }
    }
  }

  private flagValueMeansEnabled(value: any): boolean {
    return value ? true : false;
  }

  private flagValueIsAmongRecommended(value: any): boolean {
    if (value === true || value === false ) { return true; }
    return false;
  }

  /**
   * Once initial feature flags data is obtained, import them. \
   * (Only in non-production (UI dev env), import overrides from app-config)
   */
  private initialize(flags: object, importSourceInfo: string): void {
    // Fetch and map succeeded, update flags from this data.
    this.updateFlags(flags, importSourceInfo);
    if (!this.env.production) {
      if (this.featureFlagsFromConfig && typeof this.featureFlagsFromConfig === 'object') {
        this.updateFlags(this.featureFlagsFromConfig, 'UI Dev Env Flag Override');
        console.log(`Flags has been imported from [ui/src/static/app-config.json]`
                      + ` and might override features specified by Helen.`);
        console.log(JSON.stringify(this.featureFlagsFromConfig));
      }
    }
  }

}
