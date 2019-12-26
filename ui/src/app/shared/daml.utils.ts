/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Observable } from 'rxjs';
import * as JSZip from 'jszip';

const fieldDelim = ':'; // What delimiter to use when flattening DAML structure
const objDelim = '.'; // What delimiter is used to denote DAML object hierarchy
const camelFieldToWords = true;
const stepMinDelay = 1600; // Mininum delay for eash step to make sure extraction UI doesn't blitz thru the process

/**
 * Extraction events enum
 */
export enum DamlDarExtractionType {
  unzipStart = 'unzipStart',
  unzipEnd = 'unzipEnd',
  metaInf = 'metaInf',
  sourceFile = 'sourceFile',
  sourceAll = 'sourceAll',
  abi = 'abi',
  finished = 'finished',
}

/**
 * Extraction event object with data
 * type: what kind of extraction event
 * data: event data
 */
export interface DamlDarExtractionEvent {
  type: DamlDarExtractionType;
  data?: any;
}

/**
 * Checker function for type string is of DAML primitive type
 * https://github.com/digital-asset/daml/blob/master/daml-lf/spec/daml-lf-1.rst#kinds-types-and-expressions
 */
const damlPrimitives = [
  'TArrow',                                  // BTArrow: Arrow type
  'Int64',                                   // BTyInt64: 64-bit integer
  'Numeric',                                 // BTyNumeric: numeric, precision 38, parametric scale between 0 and 37
  'Text',                                    // BTyText: UTF-8 string
  'Date',                                    // BTyDate
  'Timestamp',                               // BTyTime: UTC timestamp
  'Party',                                   // BTyParty
  'Date',                                    // BTyDate: year, month, date triple
  'Unit',                                    // BTyUnit
  'Bool',                                    // BTyBool
  'List',                                    // BTyList
  'Optional',                                // BTyOptional
  'TextMap',                                 // BTTextMap: map with string keys
  'GenMap',                                  // BTGenMap: map with general value keys
  'Update',                                  // BTyUpdate
  'ContractId',                              // BTyContractId
  'Any',                                     // BTyAny
  'TypeRep',                                 // BTTypeRep
];
export function damlPrimitive(type: string) {
  if (damlPrimitives.indexOf(type) >= 0) {
    return true;
  } else if (type && type.startsWith('[') && type.endsWith(']')) {
    return true; // is array type (e.g. [Party], [Text], etc), still 'primitive'
  }
  return false;
}

/**
 * Flatten DAML structure until object is made with all primitives
 */
export function damlFlattenStruct(constructorArgs, strucMap) {
  constructorArgs = JSON.parse(JSON.stringify(constructorArgs));
  for (let i = 0; i < constructorArgs.length; ++i) {
    const input = constructorArgs[i];
    if (!damlPrimitive(input.type) && strucMap[input.type]) {
      const typeCopy = JSON.parse(JSON.stringify(strucMap[input.type]));
      for (let j = 0; j < typeCopy.inputs.length; ++j) {
        typeCopy.inputs[j].name = input.name + objDelim + typeCopy.inputs[j].name;
      }
      constructorArgs.splice(i, 1, ...typeCopy.inputs);
      --i;
    }
  }
  return constructorArgs;
}

/**
 * Active contracts
 * Table Header - get property name for UI display
 */
export function damlGetPropertyName(inputName: string) {
  if (camelFieldToWords) {
    return inputName.split(objDelim).map(a => {
      // 'camelCaseToWords' ===> 'Camel Case To Words'
      a = a.replace( /([A-Z])/g, ' $1' );
      return a.charAt(0).toUpperCase() + a.slice(1);
    }).join(fieldDelim);
  }
  return inputName;
}

/**
 * Active contracts
 * Table Content - get property valu fot UI display
 */
export function damlGetPropertyData(damlTransaction, inputName: string, raw?: boolean) {
  const travelPath = inputName.split(objDelim);
  let at = damlTransaction.argument;
  while (travelPath.length > 0) {
    at = at[travelPath.shift()];
    if (at === undefined) { return null; }
  }
  if (raw) { return at; }
  if (at === undefined || at === null) {
    return '';
  } else if (Array.isArray(at)) {
    return at.join(', ');
  } else if (typeof at === 'object') {
    return JSON.stringify(at);
  } else {
    return at + '';
  }
}

/**
 * Extract basic metadata from DAT
 */
export function damlExtractDataFromDar(blob: Blob, instant: boolean = false): Observable<any> {
  return new Observable<DamlDarExtractionEvent>(ob => {
    ob.next({ type: DamlDarExtractionType.unzipStart });
    JSZip.loadAsync(blob).then(async unzipped => {

      if (!instant) { await sleep(stepMinDelay); }
      ob.next({ type: DamlDarExtractionType.unzipEnd });
      let metadata;

      const files = unzipped.files;
      const fileKeys = Object.keys(files);

      const metaInfFileKey = 'META-INF/MANIFEST.MF';
      const metaInfText = await files[metaInfFileKey].async('text');
      const metaInfMap = damlExtractMetadataKeyValue(metaInfText);

      const confFileKey = fileKeys.filter(name => name.endsWith('.conf'))[0];
      const confText = await files[confFileKey].async('text');
      const confMap = damlExtractMetadataKeyValue(confText);
      const exposedModules = confMap['exposed-modules'].split(' ');
      delete confMap['exposed-modules'];

      const damlFiles = fileKeys.filter(name => name.endsWith('.daml'));

      metadata = confMap;
      metadata['binary-length'] = blob.size;
      metadata['file-count'] = { total: fileKeys.length, daml: damlFiles.length };
      metadata['compiler'] = `${metaInfMap['Created-By']} ${metaInfMap['Sdk-Version']}`;
      metadata['exposed-modules'] = exposedModules;

      if (!instant) { await sleep(stepMinDelay); }
      ob.next({ type: DamlDarExtractionType.metaInf, data: JSON.parse(JSON.stringify(metadata)) });


      /**
       * TODO: Keep pushing DA for supporting function ABI support in the metadata via Ledger API
       * TODO: Maintaining a full Haskell/DAML parser on our side is NOT manageable
       */
      metadata.output = { abi: [] };
      const abi = metadata.output.abi;
      const sourceList = [];
      for (const filename of damlFiles) {
        const fileContent = await damlAddFileToSourceCode(filename, files, sourceList);
        const fileTypeMap = damlTypeMapFile(fileContent);
        if (fileTypeMap && Object.keys(fileTypeMap).length > 0) {
          for (const templateName of Object.keys(fileTypeMap)) {
            const templateInfo = fileTypeMap[templateName];
            abi.push({ name: templateInfo.name, type: '', inputs: templateInfo.properties });
          }
        }
        ob.next({ type: DamlDarExtractionType.sourceFile, data: { name: filename, content: fileContent } });
        if (!instant) { await sleep(0); } // give browser time to breathe, and not freeze
      }


      ob.next({ type: DamlDarExtractionType.sourceAll, data: sourceList });

      ob.next({ type: DamlDarExtractionType.abi, data: metadata.output.abi });
      if (!instant) { await sleep(stepMinDelay); }

      ob.next({ type: DamlDarExtractionType.finished, data: metadata });
    });
  });
}

export function damlExtractMetadataKeyValue(text) {
  const props = [];
  for (const part of text.split('\n')) {
    if (!part) { continue; }
    if (part[0] === ' ') { props[props.length - 1] += part.substr(1); } else { props.push(part); }
  }
  const kvMap = {};
  for (const prop of props) {
    const colonPos = prop.indexOf(': ');
    const propname = prop.substr(0, colonPos);
    const propline = prop.substr(colonPos + 2);
    const propdata = propline.indexOf(', ') === -1 ? propline : propline.split(', ');
    kvMap[propname] = propdata;
  }
  return kvMap;
}

export function damlTypeMapFile(file: string) {
  let lines = file.split('\n');
  if (lines.filter(line => line.startsWith('module ')).length === 0) { return null; }
  lines = damlCommentReplace(lines);
  lines = damlEmptyLineReplace(lines);
  lines = damlTypeReplace(lines);
  lines = damlImportReplace(lines);
  lines = damlModuleLocalReplace(lines);
  lines = damlPurgeReplacers(lines);

  return damlTemplateParse(lines);
}

/**
 * ! Ephemeral
 * JWT Token for Accessing JSON-API, for showcasing the json-api interaction
 * Production Ledger API will have different authentication token
 */
export function damlJsonApiHeaders() {
  return {
    'Authorization': 'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.'
                      + 'eyJsZWRnZXJJZCI6Ik15TGVkZ2VyIiwiYXBwbGljYXRpb'
                      + '25JZCI6ImZvb2JhciIsInBhcnR5IjoiQWxpY2UifQ.'
                      + '4HYfzjlYr1ApUDot0a6a4zB49zS_jrwRUOCkAiPMqo0',
    'Content-Type': 'application/json',
  };
}




function damlAddFileToSourceCode(filename, files, sourcecode) {
  return files[filename].async('text').then(text => {
    filename = filename.split('/'); if (filename.length > 1) { filename.shift(); }
    filename = filename.join('/');
    let filetext = text;
    if (sourcecode.length > 0) { filetext = `\n\n\n\n\n` + filetext; }
    sourcecode.push(filetext);
    return text;
  });
}


async function sleep(ms = 0) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve(true);
    }, ms);
  });
}

























/**
 * ! Temporary
 *
 * [PARSER SECTION BEGIN]
 * Below are rudimentary Haskell/DAML parser functions to showcase
 * what is possible with metadata parsing, this below will be removed
 * once DAML starts to natively provide metadata (function ABI) for
 * thre requested DAR packages.
 *
 * https://github.com/digital-asset/daml/issues/3883
 *
 */

function damlFindReplaceWord(line, match, replace) {
  try {
    if (match.trim().length === 0) { return line; }
    let pos = 0;
    let at;
    let count = 0;
    while ((at = line.indexOf(match, pos)) >= 0 && ++count < 100) {
      const pch = line[at - 1];
      const pvalid = (pch !== '.' && isWordBoundary(pch));
      const lch = line[at + match.length];
      const lvalid = isWordBoundary(lch);
      if (pvalid && lvalid) {
        line = line.substr(0, at) + line.substr(at).replace(match, replace);
        pos = at + replace.length + 1;
      } else {
        pos = at + match.length;
      }
    }
    if (count >= 100) {
      console.log(`Overflowed: ${match} ||| ${replace} ||| ${line}`);
    }
  } catch (e) {
    // console.log(e);
    // console.log(line, match, replace);
  }
  return line;
}

function damlModuleLocalReplace(lines) {
  let moduleName;
  const templateNames = [];
  lines.forEach((line) => {
    if (!moduleName) {
      if (line.indexOf('module ') === 0) {
        moduleName = line.split('module ')[1].split(' ')[0].trim();
      }
    }
    if (line.indexOf('template ') === 0) {
      const templateName = line.split('template ')[1].split(' ')[0].trim();
      templateNames.push(templateName);
    }
  });
  if (moduleName) {
    lines.forEach((line, j) => {
      if (line.indexOf('template ') !== 0 && line.indexOf('module ') !== 0) {
        // console.log(line);
        for (const templateName of templateNames) {
          // console.log(line, ' ====== ', templateName, moduleName + '.' + templateName);
          line = lines[j] = damlFindReplaceWord(line, templateName, moduleName + '.' + templateName);
        }
        // console.log(lines[j]);
      }
    });
  }
  return lines;
}

function damlTypeReplace(lines) {
  const typeReplacer = [];
  lines.forEach((line, j) => {
    if (typeReplacer.length > 0) {
      for (const repl of typeReplacer) {
        line = lines[j] = damlFindReplaceWord(line, repl.name, repl.value);
      }
    }
    if (line.indexOf('type ') === 0) {
      const typeDecl = line.split('type ')[1].split(' = ');
      typeReplacer.push({
        name: typeDecl[0],
        value: typeDecl[1]
      });
    }
  });
  return lines;
}

function damlImportReplace(lines) {
  const importReplacer = [];
  lines.forEach((line, j) => {
    if (importReplacer.length > 0) {
      for (const repl of importReplacer) {
        line = lines[j] = damlFindReplaceWord(line, repl.name, repl.value);
      }
    }
    if (line.indexOf('import ') === 0) {
      const importPhrase = line.trim().split('import ')[1];
      let importDecl = importPhrase.split(' ');
      if (importDecl.length === 1) {
        importDecl = [importDecl[0].split('.').slice(-1).pop(), importDecl[0]];
      } else if (importDecl.length === 2) {
        importDecl[1] = importDecl[1].replace(/\(|\)/g, '');
        importDecl = importDecl.reverse();
      } else if (importPhrase.indexOf(' qualified as ') >= 0) {
        importDecl = importPhrase.split(' qualified as ').reverse();
      } else {
        // console.log('???', line);
      }
      importReplacer.push({ name: importDecl[0], value: importDecl[1] });
    }
  });
  return lines;
}

function damlCommentReplace(lines) {
  lines = lines.join('\n').replace(/\{\-(.|\n)*?\-\}/g, '').split('\n');
  lines.forEach((line, j) => {
    if (line.indexOf('--') >= 0) { lines[j] = line.split('--').splice(0, 1).join('--'); }
  });
  return lines;
}

function damlEmptyLineReplace(lines) {
  lines = lines.filter(line => line.trim() !== '');
  return lines;
}

function damlPurgeReplacers(lines) {
  lines = lines.filter(line => (line.indexOf('daml ') === -1 && line.indexOf('type ') === -1 && line.indexOf('import ') === -1));
  return lines;
}

function damlTemplateParse(lines) {
  const templates = {};
  let currentTemplate;
  let moduleName;
  let underWith;
  let underWhere;
  lines.forEach((line) => {
    if (!moduleName) {
      if (line.indexOf('module ') === 0) {
        moduleName = line.trim().split('module ')[1].split(' ')[0];
      }
    }
    if (line.indexOf('template ') === 0) {
      const templateName = line.trim().split('template ')[1].split(' ')[0];
      currentTemplate = moduleName + '.' + templateName;
      templates[currentTemplate] = { name: currentTemplate, properties: [], choices: [] };
      underWith = underWhere = null;
      if (line.split(' ').indexOf('with') >= 0) { underWith = true; }
      return;
    }
    if (currentTemplate) {
      if (line.indexOf(' ') !== 0) {
        currentTemplate = underWith = underWhere = null;
      } else {
        if (!underWith && !underWhere) {
          if (line.split(' ').indexOf('with') >= 0) { underWith = true; }
        } else if (underWith) {
          if (line.split(' ').indexOf('where') >= 0) {
            underWith = false; underWhere = true;
          } else {
            damlDigestArgumentLine(line).forEach(propDef => {
              templates[currentTemplate].properties.push(propDef);
            });
          }
        } else if (underWhere) {
          templates[currentTemplate].choices.push(line);
        }
      }
    }
  });
  for (const templateName of Object.keys(templates)) {
    templates[templateName].choices = damlTemplateParseWhere(templates[templateName].choices);
  }
  return templates;
}

function damlTemplateParseWhere(whereLines) {
  let currentKeywordObject;
  const whereList = [];
  if (whereLines.length === 0) { return whereList; }
  const frontPad = whereLines[0].match(/^\s*/)[0].length;
  whereLines.forEach((line) => {
    const linePad = line.match(/^\s*/)[0].length;
    if (frontPad !== linePad) {
      if (currentKeywordObject) { currentKeywordObject.inner.push(line); }
    } else if (frontPad === linePad) { // 1st blockspace under where clause
      const split = line.trim().split(' ');
      const firstWord = split[0];
      if (currentKeywordObject) { currentKeywordObject.resolve(); }
      currentKeywordObject = null;
      switch (firstWord) {
        case 'signatory':
          whereList.push(damlTemplateWhere_signatory(line));
        break;
        case 'observer':
          whereList.push(damlTemplateWhere_observer(line));
        break;
        case 'controller':
          if (split.indexOf('can') === 2) {
            const party = split[1];
            currentKeywordObject = damlTemplateWhere_controller(party);
            whereList.push(currentKeywordObject);
          }
        break;
        default:
          // console.log('Undigestable template where clause line: ', line);
        break;
      }
    }
  });
  if (currentKeywordObject && currentKeywordObject.resolve) { currentKeywordObject.resolve(); }
  return whereList;
}

function damlTemplateWhere_signatory(line) {
  let signatories = line.split('signatory ')[1].split(',');
  signatories = signatories.map(item => item.trim());
  return { type: 'signatory', parties: signatories};
}

function damlTemplateWhere_observer(line) {
  let signatories = line.split('observer ')[1].split(',');
  signatories = signatories.map(item => item.trim());
  return { type: 'observer', parties: signatories};
}

function damlTemplateWhere_controller(party) {
  return { type: 'controller', party: party, inner: [], resolve: function() {
    this.choices = damlTemplateWhere_controller_parse(this.inner);
    delete this.inner;
    delete this.resolve;
  }};
}

function damlTemplateWhere_controller_parse(innerLines) {
  let currentKeywordObject;
  const choices = {};
  if (innerLines.length === 0) { return choices; }
  const frontPad = innerLines[0].match(/^\s*/)[0].length;
  innerLines.forEach((line) => {
    const linePad = line.match(/^\s*/)[0].length;
    if (frontPad === linePad) { // 1st blockspace under controller clause
      if (currentKeywordObject) { currentKeywordObject.resolve(); }
      currentKeywordObject = damlTemplateWhere_controller_choice(line.trim());
      choices[currentKeywordObject.name] = currentKeywordObject;
    } else {
      if (currentKeywordObject) { currentKeywordObject.inner.push(line.trim()); }
    }
  });
  if (currentKeywordObject && currentKeywordObject.resolve) { currentKeywordObject.resolve(); }
  return choices;
}

function damlTemplateWhere_controller_choice(line) {
  const name = line.split(':')[0].trim();
  return { type: 'choice', name: name, params: [], inner: [line], resolve: function() {
    damlTemplateWhere_controller_choice_resolve(this, this.inner);
    delete this.with;
    delete this.do;
    delete this.inner;
    delete this.resolve;
  }};
}

function damlTemplateWhere_controller_choice_resolve(choiceObj, innerLines) {
  innerLines.forEach((line) => {
    if (choiceObj.do) { return; }
    if (!choiceObj.returns) {
      if (line.indexOf(':') >= 0) {
        const afterName = line.substr(line.indexOf(':') + 1);
        if (afterName.indexOf(' with') >= 0) {
          choiceObj.returns = afterName.split(' with')[0].trim();
          line = afterName.split(' with').slice(1).join(' with');
        } else {
          choiceObj.returns = afterName.trim();
        }
      }
    }
    if (!choiceObj.with) {
      if (line.indexOf('with') >= 0 && line.split(' ').map(item => item.trim()).indexOf('with') >= 0) {
        const split = line.split(' ');
        line = split.slice(split.indexOf('with') + 1).join(' ');
        choiceObj.with = true;
      }
    }
    if (line.indexOf('do') >= 0 && line.split(' ').map(item => item.trim()).indexOf('do') >= 0) {
      const split = line.split(' ');
      line = split.slice(split.indexOf('do') + 1).join(' ');
      choiceObj.do = true; choiceObj.with = false;
    }
    if (choiceObj.with) {
      damlDigestArgumentLine(line).forEach(argDef => {
        choiceObj.params.push(argDef);
      });
    }
  });
}

function isWordBoundary(ch) {
  return (!ch || ch === ' ' || ch === '(' || ch === ')' || ch === ',');
}

function damlDigestArgumentLine(line) {
  line = line.trim(); if (!line) { return []; }
  const args = [];
  const argsSplit = line.split(',');
  for (let arg of argsSplit) {
    arg = arg.trim();
    let propDef = arg.split(':');
    propDef = propDef.map(item => item.trim());
    args.push({ name: propDef[0], type: propDef[1] });
  }
  return args;
}

/**
 * ! Temporary
 * [PARSER SECTION END]
 */
