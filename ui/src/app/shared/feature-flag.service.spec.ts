/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { TestBed, inject } from '@angular/core/testing';
import { FeatureFlagService } from './feature-flag.service';

const groupers = ['('];
const unaryOperators = ['!', 'not'];
const binaryOperators = ['&&', '||', 'and', 'or'];
const recognizedFlagNames = ['A', 'B', 'C', 'D.E', 'F.G.H', 'I.J.K.L'];
const unrecognizedFlagNames = ['unrecog_U', 'A.unrecog_V', 'D.E.unrecog_W', 'unrecog_X', 'unrecog_Y', 'unrecog_Z'];

const range = (min, max) => (Math.floor(Math.random() * (max + 1 - min)) + Math.floor(min));
const shuffleArray = (arr: any[]) => {
    // Fisher-Yates Shuffle
    for (let i = arr.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
    return arr;
};
const pickRandom = (arr: any[], pickCount = 1) => {
  if (pickCount === 1) {return arr[Math.floor(Math.random() * arr.length)]; }
  const picks = [];
  if (pickCount > 1) {
    for (let i = 0; i < pickCount; ++i) {
      picks.push(arr[Math.floor(Math.random() * arr.length)]);
    }
  }
  return picks;
};
const onChance = (percent: number): boolean => (Math.random() <= percent / 100);
const randomFlagValue = (truthy, falsey) => {
  if (onChance(8)) { return onChance(50) ? 'on; some-meta=1' : 'off; some-meta=2';
  } else { return onChance(50) ? pickRandom(truthy) : pickRandom(falsey); }
};
const flagsShouldReturn = (flags, service: FeatureFlagService) => {
  for (const flagName of flags) {
    if (service.getFlagBooleanValue(flagName) === false) {
      return false;
    }
  }
  return true;
};
const makeRandomFlags = (count: number = 5, mixAtLeastOneUnrecognized = false) => {
  const flags = [];
  if (mixAtLeastOneUnrecognized) {
    flags.push( pickRandom(unrecognizedFlagNames) );
    for (let i = 1; i < count; ++i) {
      flags.push( onChance(50) ? pickRandom(recognizedFlagNames) : pickRandom(unrecognizedFlagNames) );
    }
  } else {
    for (let i = 0; i < count; ++i) { flags.push(pickRandom(recognizedFlagNames)); }
  }
  return shuffleArray(flags);
};
const makeRandomFlagsMap = () => {
  const map = {};
  for (const flagName of recognizedFlagNames) {
    map[flagName] = randomFlagValue(FeatureFlagService.truthy, FeatureFlagService.falsey);
  }
  return map;
};
const makeRandomExpression = (wordLength: number = 10) => {
  const list = [];
  let bracketCount = 0;
  if (onChance(30)) { list.push('('); bracketCount++; }
  let wordPicked = false; let binOpPicked = false; let unaryPicked = true;
  while (list.length < wordLength) {
    if (wordPicked) {
      if (onChance(80)) {
        list.push(pickRandom(binaryOperators)); binOpPicked = true; wordPicked = false;
      } else if (!unaryPicked && bracketCount > 0 ) {
        list.push(')'); bracketCount--;
      }
      unaryPicked = false;
    } else {
      if (onChance(25)) {
        list.push(pickRandom(unaryOperators)); wordPicked = false; unaryPicked = true;
      } else {
        if (onChance(75)) {
          list.push(pickRandom(recognizedFlagNames)); wordPicked = true; if (binOpPicked) { binOpPicked = false; }
        } else {
          list.push(pickRandom(groupers)); bracketCount++;
          wordPicked = false;
        }
        unaryPicked = false;
      }
    }
  }
  const lastWord = list[list.length - 1];
  if (unaryOperators.indexOf(lastWord) >= 0) { list.push(pickRandom(recognizedFlagNames)); }
  if (binaryOperators.indexOf(lastWord) >= 0) { list.push(pickRandom(recognizedFlagNames)); }
  while (bracketCount > 0) { list.push(')'); bracketCount--; }
  let result = list.join(' ');
  result = result.replace(/\( \)/g, pickRandom(recognizedFlagNames));
  // Randomize spacings
  result = onChance(33.3) ? result.replace(/ ! /g, '!') :
            onChance(50) ? result.replace(/ ! /g, '! ') : result.replace(/ ! /g, ' !');
  result = onChance(33.3) ? result.replace(/ \( /g, '(') :
            onChance(50) ? result.replace(/ \( /g, '( ') : result.replace(/ \( /g, ' (');
  result = onChance(33.3) ? result.replace(/ \) /g, ')') :
            onChance(50) ? result.replace(/ \) /g, ') ') : result.replace(/ \) /g, ' )');
  return result;
};

fdescribe('Service: FeatureFlag', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [FeatureFlagService]
    });
  });

  fit('should ...', inject([FeatureFlagService], (service: FeatureFlagService) => {

    let t1, t2; t1 = Date.now();

    FeatureFlagService.suppressErrorLogs = true;
    FeatureFlagService.suppressWarningLogs = true;

    const verbose = false;
    const performanceCheck = false;
    const performanceIterations = 100000;

    const simpleUnrecognizedTestCount = 500;
    const simpleRecognizedTestCount = 500;
    const complexRecognizedTestCount = 500;

    for (const flagName of recognizedFlagNames) { service.setFlag(flagName, true); }

    expect(service).toBeTruthy();

    if (performanceCheck) {
      let p1, p2;

      // Override iterations
      p1 = Date.now();
      for (let i = 0; i < performanceIterations; ++i) {
        service.checkFlagsCondition([true, 'some string']);
      }
      p2 = Date.now();
      console.log(`FeatureFlagService 100k debug override iterations: ${p2 - p1} ms.`);

      // SIMPLE iterations
      const randomFlags = [];
      for (let i = 0; i < performanceIterations; ++i) {
        randomFlags.push( makeRandomFlags(range(1, 3)) );
      }
      p1 = Date.now();
      for (let i = 0; i < performanceIterations; ++i) {
        service.checkFlagsCondition( randomFlags[i] );
      }
      p2 = Date.now();
      console.log(`FeatureFlagService 100k SIMPLE iterations (1 ~ 3 random flags): ${p2 - p1} ms.`);

      // COMPUTED iterations, suffers from 4+ word expressions
      const randomExpressions = [];
      for (let i = 0; i < performanceIterations; ++i) {
        randomExpressions.push( [ 'COMPUTE', makeRandomExpression(range(1, 3)) ] );
      }
      p1 = Date.now();
      for (let i = 0; i < performanceIterations; ++i) {
        // if (i < 100) { console.log(randomExpressions[i][1]); }
        service.checkFlagsCondition( randomExpressions[i] );
      }
      p2 = Date.now();
      console.log(`FeatureFlagService 100k COMPUTE iterations (1 ~ 3 ramdom word expressions): ${p2 - p1} ms.`);
    }

    if (verbose) {
      console.log('------------------------------------------------------------');
      console.log('Testing debug overrides (true / false) ');
      console.log('------------------------------------------------------------');
    }
    let exptectedOverrideValue; let computedOverrideValue;

    exptectedOverrideValue = true;
    computedOverrideValue = service.checkFlagsCondition(true); // must override to true
    expect(computedOverrideValue).toBe(exptectedOverrideValue);
    if (verbose) { console.log(`OVERRIDE with true, expected ${exptectedOverrideValue}, got ${computedOverrideValue}`); }


    exptectedOverrideValue = false;
    computedOverrideValue = service.checkFlagsCondition(false); // must override to false
    expect(computedOverrideValue).toBe(exptectedOverrideValue);
    if (verbose) { console.log(`OVERRIDE with false, expected ${exptectedOverrideValue}, got ${computedOverrideValue}`); }

    // Simple case, at least one unrecognized flags with a random value,
    // all others are known flags with random value. Must reject all.
    if (verbose) {
      console.log('------------------------------------------------------------');
      console.log('Testing SIMPLE case with at least one unrecognized flag...');
      console.log('------------------------------------------------------------');
    }
    for (let i = 0; i < simpleUnrecognizedTestCount; ++i) {
      const mixAtLeastOneUnrecognized = true;
      const randomFlags = makeRandomFlags(range(1, 10), mixAtLeastOneUnrecognized);
      if (verbose) { console.log('flags: ', JSON.stringify(randomFlags)); }
      const flagsKVMap = makeRandomFlagsMap();
      if (verbose) { console.log('flag values: ', JSON.stringify(flagsKVMap)); }
      service.updateFlags(flagsKVMap);
      const expectedValue = false;
      const computedValue = service.checkFlagsCondition(randomFlags);
      if (verbose || computedValue !== expectedValue) { console.log(`SIMPLE 1, expected ${expectedValue}, got ${computedValue}`); }
      expect(computedValue).toBe(expectedValue);
    }


    // Simple case, random expression all recognized flags with random values
    if (verbose) {
      console.log('------------------------------------------------------------');
      console.log('Testing SIMPLE case with all recognized flags, random values...');
      console.log('------------------------------------------------------------');
    }
    for (let i = 0; i < simpleRecognizedTestCount; ++i) {
      const randomFlags = makeRandomFlags(range(1, 10));
      if (verbose) { console.log('flags: ', JSON.stringify(randomFlags)); }
      const flagsKVMap = makeRandomFlagsMap();
      if (verbose) { console.log('flag values: ', JSON.stringify(flagsKVMap)); }
      service.updateFlags(flagsKVMap);
      const expectedValue = flagsShouldReturn(randomFlags, service);
      const computedValue = service.checkFlagsCondition(randomFlags);
      if (verbose || computedValue !== expectedValue) { console.log(`SIMPLE 2, expected ${expectedValue}, got ${computedValue}`); }
      expect(computedValue).toBe(expectedValue);
    }


    // Complex case, random expression with all recognized flags with random values
    if (verbose) {
      console.log('------------------------------------------------------------');
      console.log('Testing COMPUTE case with recognized flags, random values...');
      console.log('------------------------------------------------------------');
    }
    for (let i = 0; i < complexRecognizedTestCount; ++i) {
      const expression = makeRandomExpression(range(1, 30));
      if (verbose) { console.log('expression: ', JSON.stringify(expression)); }
      const flagsKVMap = makeRandomFlagsMap();
      if (verbose) { console.log('flag values: ', JSON.stringify(flagsKVMap)); }
      service.updateFlags(flagsKVMap);
      const logicalExpression = service.getSafeLogicalExpression(expression);
      if (verbose) { console.log('parsed: ', JSON.stringify(logicalExpression)); }
      // tslint:disable:no-eval
      let result; result = ''; eval('result = ' + logicalExpression);
      const expectedValue = result;
      const computedValue = service.checkFlagsCondition(['COMPUTE', expression]);
      if (verbose || computedValue !== expectedValue) { console.log(`COMPUTE, expected ${expectedValue}, got ${computedValue}`); }
      expect(computedValue).toBe(expectedValue);
    }

    t2 = Date.now();

    console.log(`FeatureFlagService all tests have taken ${t2 - t1} ms.`);
  }));


});
