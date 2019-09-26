/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { Injectable, TemplateRef } from '@angular/core';
import { from, of } from 'rxjs';
import { map, catchError } from 'rxjs/operators';

/**
 * Most of the time flag condition compute requests will come from directives,
 * This helps with which Angular component tempplate is throwing error and under which element tag
 */
export interface FlagDirectiveSource { componentName?: string; parentTagName?: string; }

/**
 * Any flag will have its designated `name`, `booleanValue` (current on/off state).
 * Some flags are nested and will have a `parent` container object.
 */
export interface FlagData { name: string; booleanValue: boolean; parent?: object; }




@Injectable({
  providedIn: 'root'
})
export class FeatureFlagService {

  public static suppressWarningLogs: boolean = false;
  public static suppressErrorLogs: boolean = false;

  public static truthy: any[] = [true];
  public static falsey: any[] = [false];

  private importSource: string; // path of feature flags file (overridable in app-config.json)
  private featureFlags: {} = {}; // Key-value map from JSON file (notice: values not necessarily true/false)
  private featureFlagsBooleanValues: {} = {}; // strictly boolean

  private coerceFlagValues: boolean = false;

  private computeResultCache: Map<string, boolean> = new Map<string, boolean>();

  constructor() {}

  /* Getters */

  get allFlagsMap(): {} { return JSON.parse(JSON.stringify(this.featureFlags)); }

  get allFlagsBooleanValuesMap(): {} { return JSON.parse(JSON.stringify(this.allFlagsBooleanValuesMap)); }

  get featureFlagsSource(): string { return this.importSource; }


  /**
   * Is the flag with this flag name on right now?
   */
  getFlagBooleanValue(flagName: string): boolean {
    const flagData = this.findFlagByName(flagName);
    if (flagData === null) { return null; }
    return flagData.booleanValue;
  }

  /**
   * (awaitable) Get feature flag file from local or web source. \
   * and import into this service. (Called from `app.init.ts`)
   */
  importFromSource(featureFlagsSource: string = null): Promise<void> {
    if (!featureFlagsSource) { featureFlagsSource = 'static/feature-flag.json'; }
    return from(
        fetch(featureFlagsSource) .then((response) => response.json())
      ).pipe(

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
            console.warn('Falling back to default JSON feature flags...');
          }

          // Still load the app with default feature flags
          return of({});
        }),

        map((flags) => {
          // Fetch and map succeeded, update flags from this data.
          this.updateFlags(flags, featureFlagsSource);
        })

      ).toPromise();
  }

  /**
   * Resets all flags, value maps, and compute caches \
   * and brings back to the default flag values
   */
  reset() {
    this.featureFlags = {};
    this.featureFlagsBooleanValues = {};
    this.computeResultCache = new Map<string, boolean>();
  }

  /* Toggle flag values coercion */
  setCoerceFlagValues(value: boolean) {
    this.setFlag('coerceFlagValues', value);
    this.coerceFlagValues = value ? true : false;
    if (value) {
      // handle loose, syntatically reasonable values as well
      FeatureFlagService.truthy = ['on', 'true', true, 1, '1', 'yes'];
      FeatureFlagService.falsey = ['off', 'false', '', false, '0', 0, null, 'null', undefined, 'undefined', 'no'];
    } else {
      // Only true and false accepted by default
      FeatureFlagService.truthy = [true];
      FeatureFlagService.falsey = [false];
    }
  }

  /**
   * Setting a flag by flag name and value \
   * Flag name can dot-concat to denote nested. (e.g. daml.test.flag1) \
   * You can only set flags listed in `featureFlagRegistry`
   */
  setFlag(flagName: string, flagValue: any): boolean {
    try {
      const boolValue = this.flagValueMeansEnabled(flagValue);
      this.dotTravelSet(this.featureFlags, flagName, flagValue, true);
      this.dotTravelSet(this.featureFlagsBooleanValues, flagName, boolValue, true);
      return true;
    } catch (e) {
      if (!FeatureFlagService.suppressErrorLogs) {
        console.error(e);
        return false;
      }
    }
  }

  /**
   * Called to initialize from `importFromSource`. \
   * You runtime override any flags
   */
  updateFlags(flags: Object, importSource: string = null) {
    if (importSource !== null) { this.importSource = importSource; }
    this.flagsRecursiveImport(flags, this.featureFlags, this.featureFlagsBooleanValues);
    this.computeResultCache = new Map<string, boolean>(); // Purge cache
  }

  /**
   * Checks flags expression, possibly from a directive \
   * This ultimately determines whether an element is shown or not.
   *
   * Sadly, 'any' is used below because Angular directives don't care about types \
   * it will let you pass whatever JS expression in the template.
   */
  checkFlagsCondition(flags: any | any[], templateRef: TemplateRef<any> = null): boolean {

    if (flags === true) { return true; }
    if (flags === false) { return false; }

    // templateReference where the directive call resides, used for debugging
    let sourceComponent: FlagDirectiveSource = null;
    if (templateRef) {
      try {
        sourceComponent = {
          componentName : templateRef['_parentView'].component.constructor.name,
          parentTagName : templateRef.elementRef.nativeElement.parentElement.localName,
        };
      } catch (e) { console.error(e); }
    }

    const wasArray = Array.isArray(flags);
    if (!wasArray) { flags = [flags]; }

    // Force cast into string[]
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
    if (!this.getFlagBooleanValue('autoComputeFlags') && !overrideToUseCompute) {

      for (const flagName of flags) {
        const flagData = this.findFlagByName(flagName);
        if (flagData !== null) {
          if (!flagData.booleanValue) { return false; }
        } else {
          if (!FeatureFlagService.suppressErrorLogs) {
            if (sourceComponent === null) {
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
      if (this.getFlagBooleanValue('cacheDirectiveExpressions')) {
        const cached = this.computeResultCache.get(flagsExpression);
        if (cached !== undefined) { return cached; }
      }
      const computeSafeLogicalExpression = this.getSafeLogicalExpression(flagsExpression, sourceComponent);
      if (computeSafeLogicalExpression === null) { return false; }

      let result: boolean; result = false;
      try {
        // tslint:disable:no-eval
        eval(`result = (${computeSafeLogicalExpression});`);
        if (this.getFlagBooleanValue('cacheDirectiveExpressions')) {
          this.computeResultCache.set(flagsExpression, result);
        }
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
  getSafeLogicalExpression(flagsExpression: string, sourceComponent: FlagDirectiveSource = null): string {

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
      const flagData = this.findFlagByName(word);
      if (flagData !== null) {
        expressionWords[i] = flagData.booleanValue ? 'true' : 'false';
      } else {
        if (!FeatureFlagService.suppressErrorLogs) {
          if (sourceComponent === null) {
            throw new Error(`FeatureFlagService: unrecognized feature flag '${word}'`);
          } else {
            throw new Error(`FeatureFlagService: unrecognized feature flag '${word}'`
              + ` in template '${sourceComponent.componentName}'`
              + ` in a child element of '${sourceComponent.parentTagName}'`);
          }
        }
      }
    });
    return expressionWords.join(' ');

  }

  // Resolve dot separated name scheme in flags ('daml.contracts.flag1')
  private findFlagByName(flagName: string): FlagData {
    try {
      const result = this.dotTravel(this.featureFlagsBooleanValues, flagName);
      return {name: flagName, booleanValue: result.node ? true : false, parent: result.prevNode};
    } catch (e) { return null; }
  }

  private flagValueMeansEnabled(value): boolean {
    if (!this.coerceFlagValues) {
      return value ? true : false;
    } else {
      if (typeof value === 'string') {
        if (value[0] === 'o') {
          if (value.indexOf('on') === 0) { return true; }
          if (value.indexOf('off') === 0) { return false; }
          if (value.indexOf('on;') === 0) { return true; }
          if (value.indexOf('off;') === 0) { return false; }
        }
        switch (value) {
          case 'no': case 'false': case '0': case 'null': case 'undefined':
            return false;
        }
      }
      return value ? true : false;
    }
  }

  private flagValueIsAmongRecommended(value): boolean {
    if (!this.coerceFlagValues) {
      if ( value === true || value === false ) {return true; }
    } else {
      if ( value === true || value === false ) {return true; }
      if (typeof value === 'string') {
        if (value[0] === 'o') {
          if (value.indexOf('on') === 0) { return true; }
          if (value.indexOf('off') === 0) { return true; }
          if (value.indexOf('on;') === 0) { return true; }
          if (value.indexOf('off;') === 0) { return true; }
        }
      }
    }
    return false;
  }

  // Travels object tree like 'daml.contract.prop' returns value `at` path `target` parent object, and last key
  private dotTravel(obj: object, path: string, force: boolean = false): {node: any, prevNode: object, lastKey: string} {
    const keys = path.split('.');
    let node = obj; let prevNode = null; let lastKey = null;
    let i = 0;
    for (const key of keys) { // travel
      if (key === '') { continue; }
      if (node === null || node === undefined) {
        throw new Error(`dotTravel cannot travel anymore at ${lastKey} (path: ${path})`);
      }
      prevNode = node; lastKey = key;
      if (node[key] === undefined) {
          if (force) {
            if (i + 1 < keys.length) { node[key] = {}; }
          } else {
            throw new Error(`dotTravel path ended abruptly at ${lastKey} (path: ${path})`);
          }
      }
      node = node[key];
      ++i;
    }
    return {node: node, prevNode: prevNode, lastKey: lastKey};
  }
  // Travels object tree like 'some.inner.path' and sets value at that point
  private dotTravelSet(obj: object, path: string, value: any, forcePath: boolean = false) {
    try {
      const travel = this.dotTravel(obj, path, forcePath);
      travel.prevNode[travel.lastKey] = value;
    } catch (e) { console.error(e); }
  }

  private flagsRecursiveImport(valueMap: object, target: object, targetBooleans: object, path: string = null) {
    if (path === null) {
      if (valueMap.hasOwnProperty('coerceFlagValues')) {
        this.setCoerceFlagValues(valueMap['coerceFlagValues'] ? true : false);
      }
    }
    for (const key of Object.keys(valueMap)) {
      const pathNow = (path === null) ? key : path + '.' + key;
      const value = valueMap[key];
      if (key.indexOf('.') >= 0) { this.setFlag(key, value); continue; }
      if (value && typeof value === 'object') {
        if (!target[key]) { target[key] = {}; }
        if (!targetBooleans[key]) { targetBooleans[key] = {}; }
        this.flagsRecursiveImport(value, target[key], targetBooleans[key], pathNow);
      } else {
        const boolValue = this.flagValueMeansEnabled(value) ? true : false;
        target[key] = value;
        targetBooleans[key] = boolValue;
        if (!FeatureFlagService.suppressWarningLogs && !this.flagValueIsAmongRecommended(value)) {
            console.warn(new Error(`FeatureFlagService: ${pathNow} not using recommended value (got '${value}')`));
        }
      }
    }
  }

}
