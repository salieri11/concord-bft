/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, Input, OnInit, OnChanges, SimpleChanges } from '@angular/core';

import { Transaction } from '../shared/transactions.model';
import { TransactionsService } from '../shared/transactions.service';
import { SmartContractsService, TargetContractData } from '../../smart-contracts/shared/smart-contracts.service';

import { AbiFunctionParameter, SmartContractVersion, AbiDefinition } from '../../smart-contracts/shared/smart-contracts.model';
import * as canoeSolidity from './canoe-solidity-patched';
import { mainRoutes } from '../../shared/urls.model';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { Block } from '../../blocks/shared/blocks.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

declare var require: any;
const createKeccakHash = require('keccak'); // import syntax cannot be used because of TS bug

enum TransactionInputViewType {
  default = 'default',
  utf8 = 'utf8',
  original = 'original',
}

enum SmartContractInputViewType {
  default = 'default',
  utf8 = 'utf8',
  opcode = 'opcode',
  decompile = 'decompile',
  original = 'original',
}

interface DecodedArgument {
  name: string;
  type: string;
  data: string;
}

@Component({
  selector: 'concord-transaction-details',
  templateUrl: './transaction-details.component.html',
  styleUrls: ['./transaction-details.component.scss']
})
export class TransactionDetailsComponent implements OnInit, OnChanges {

  @Input() transactionHash: string;
  @Input() showParentBlock: boolean;
  @Input() codeBlockMaxHeight: string;

  transaction: Transaction;
  transactionLink: string = '';

  parentBlock: Block;
  parentBlockLink: string;

  isSmartContract: boolean = false;
  contractNotFound: boolean = false;
  inputView: TransactionInputViewType | SmartContractInputViewType;
  inputViewName: string;
  decodedInput: string = '';

  codeHighlighterPlugins: string = '';
  codeHighlighterLanguage: string = 'solidity';

  // Associated contract information
  associatedContractLink: string;
  private contractData: SmartContractVersion;
  private contractInfo: TargetContractData;

  constructor(
    private blockchainService: BlockchainService,
    private transactionsService: TransactionsService,
    private smartContractsService: SmartContractsService,
    private blocksService: BlocksService,
    ) {
      this.chooseInputView(TransactionInputViewType.default);
    }

  ngOnInit() {
    this.loadTransaction(this.transactionHash);
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.transactionHash && changes.transactionHash.previousValue) {
      this.loadTransaction(changes.transactionHash.currentValue);
    }
  }

  loadTransaction(transactionHash: string) {
    this.transactionsService.getTransaction(transactionHash).subscribe((response) => {
      this.transaction = response;
      this.transactionLink = `/${this.blockchainService.blockchainId}`
                            + `/${mainRoutes.blocks}/${this.transaction.block_number}`
                            + `/${mainRoutes.transactions}/${this.transaction.hash}`;
      this.isSmartContract = this.transaction.contract_address ? true : false;
      this.decodeToInputView();
      if (this.showParentBlock) {
        this.blocksService.getBlock(this.transaction.block_number).subscribe(blockData => {
          this.parentBlock = blockData;
          this.parentBlockLink = `/${this.blockchainService.blockchainId}`
                                + `/${mainRoutes.blocks}/${this.parentBlock.number}`;
        });
      }
    });
  }

  chooseInputView(type: TransactionInputViewType | SmartContractInputViewType) {
    this.inputView = type;
    this.inputViewName = `transactions.inputView.` + type; // i18n key
    this.decodeToInputView();
  }

  async decodeToInputView() {
    if (!this.transaction || !this.transaction.input) { return; }
    const input = this.transaction.input;
    const rawInput = input.substr(2);

    this.codeHighlighterPlugins = '';
    this.decodedInput = '';

    const contractData = await this.resolveAssociatedContract();
    this.contractNotFound = (!contractData) ? true : false;

    if (this.contractNotFound && this.isSmartContract) {
      // For 404 contracts, default raw source code view is not possible
      if (this.inputView === SmartContractInputViewType.default) {
        this.inputView = SmartContractInputViewType.decompile;
        this.inputViewName = `transactions.inputView.` + this.inputView;
      }
    }

    if (this.isSmartContract) {

      switch (this.inputView) {
        case SmartContractInputViewType.original:
            this.decodedInput = input;
          return;
        case SmartContractInputViewType.utf8:
            const parsed = String.fromCharCode.apply(null, toByteArray(rawInput));
            this.decodedInput = parsed;
          return;
        case SmartContractInputViewType.default:
            this.decodedInput = this.contractData.sourcecode;
          return;
        /* EVM package removed, needs alternative */
        // case SmartContractInputViewType.opcode:
        //     const opcodes = (new opcodeLib.EVM(input)).getOpcodes() as OpcodeData[];
        //     const lines = [];
        //     for (const opdata of opcodes) {
        //       const pushDataString = opdata.pushData ? '0x' + buf2hex(opdata.pushData) : '';
        //       const opline = `${lineNumberPad(opdata.pc)} : ${opdata.name} ${pushDataString}`;
        //       lines.push(opline);
        //     }
        //     this.decodedInput = lines.join('\n');
        //   return;
        // case SmartContractInputViewType.decompile:
        //     const EVM = (new opcodeLib.EVM(input));
        //     this.decodedInput =
        //         '=================================================\n\n'
        //       + '  FUNCTIONS:\n\n'
        //       + '=================================================\n'
        //       + EVM.getFunctions().join('\n') + '\n\n\n\n\n'
        //       + '=================================================\n\n'
        //       + '  EVENTS:\n\n'
        //       + '=================================================\n'
        //       + (EVM.getEvents().length > 0 ? EVM.getEvents().join('\n') : '(NONE)') + '\n\n\n\n\n'
        //       + '=================================================\n\n'
        //       + '  DECOMPILED:\n\n'
        //       + '=================================================\n'
        //       + EVM.decompile();
        //   return;
      }

    } else {

      switch (this.inputView) {
        case TransactionInputViewType.default:
            this.codeHighlighterPlugins = 'autolinker';
            this.decodedInput = input;
            const methodId = this.getMethodId(input);
            const contractABI = this.getContractABI();
            if (!contractABI) {
              const inputBytes = input.substr(10);
              this.decodedInput = `MethodID: 0x${methodId}  // No ABI provided; cannot parse to human-readable.\n`
                                + `InputBytes: ${inputBytes ? ('0x' + inputBytes) : (' // No input byte')}\n`;
              return;
            }

            const matchedFunction = this.findFunctionWithMethodId(methodId, contractABI);
            const decodedArguments: DecodedArgument = canoeSolidity.decodeFunctionArgs(contractABI, rawInput);

            const argsFormat = [];
            const argsHexValue = [];
            let argsIndex = 0;
            for (const arg of matchedFunction.args) {
              const argName = arg.name ? arg.name : `_`;
              argsFormat.push(`${arg.type} ${argName}`);
              const value = this.getFormattedArgumentValue(decodedArguments[argsIndex]);
              argsHexValue.push(`  ${argName} = ${value}`);
              argsIndex++;
            }
            this.decodedInput = `// MethodID: 0x${methodId}\n`
                                + `${matchedFunction.name}(${argsFormat.join(', ')})\n`
                                + `${argsHexValue.join('\n')}`;

          break;
        case TransactionInputViewType.utf8:
            const parsed = String.fromCharCode.apply(null, toByteArray(rawInput));
            this.decodedInput = parsed;
          break;
        case TransactionInputViewType.original:
            this.decodedInput = input;
          break;
      }

    }
  }

  private async resolveAssociatedContract() {
    const tx = this.transaction;
    const address = tx.contract_address ? tx.contract_address : tx.to;
    const targetContract = await this.smartContractsService.getContractByAddress(address);
    if (!targetContract) { return null; }
    this.contractInfo = targetContract;
    this.contractData = this.contractInfo.data;
    this.associatedContractLink = `/${targetContract.blockchain_id}/smart-contracts`
                                + `/${targetContract.contract_id}/versions`
                                + `/${targetContract.data.version}`;
    return targetContract;
  }

  private getMethodId(input: string) {
    return input.substr(2, 8); // remove `0x` part and return first 8 hex;
  }

  private getContractABI() {
    if (!this.contractData
      || !this.contractData.metadata
      || !this.contractData.metadata.output
      || !this.contractData.metadata.output.abi) { return null; }
    return this.contractData.metadata.output.abi;
  }

  private findFunctionWithMethodId(methodId: string, contractABI: AbiDefinition[]) {
    let funcName;
    let funcArgs: AbiFunctionParameter[];
    for (const methodDef of contractABI) {
      if (!methodDef.name) { console.log(methodDef); continue; }
      const methodArgTypes = [];
      const methodArgs: AbiFunctionParameter[] = [];
      for (const arg of methodDef.inputs) { methodArgs.push(arg); methodArgTypes.push(arg.type); }
      const methodFullName = methodDef.name + '(' + methodArgTypes.join(',') + ')';
      if (methodId === getSHA3FrontHash(methodFullName)) {
        funcName = methodDef.name;
        funcArgs = methodArgs;
        break;
      }
    }
    if (!funcName) { return null; }
    return {name: funcName, args: funcArgs};
  }

  private getFormattedArgumentValue(arg: DecodedArgument) {
    switch (arg.type) {
      case 'address':
        // TODO: implement and redirect address look up page internally
        const addressLookUpPath = `https://etherscan.io/address/0x${arg.data}`;
        return `[0x${arg.data}](${addressLookUpPath})`;
      case 'string':
          return '"' + arg.data + '"';
      default:
        return '' + arg.data;
    }
  }

}


function toByteArray(hexString) {
  const result = [];
  for (let i = 0; i < hexString.length; i += 2) { result.push(parseInt(hexString.substr(i, 2), 16)); }
  return result;
}

function getSHA3FrontHash(str: string) {
  return createKeccakHash('keccak256').update(str).digest('hex').substr(0, 8);
}
/* EVM package removed, needs alternative */
// function lineNumberPad(n) { n = n.toString(16); while ((n + '').length < 4) { n = '0' + n; } return '0x' + n; }

// function buf2hex(buffer) {
//   return Array.prototype.map.call(new Uint8Array(buffer), x => ('00' + x.toString(16)).slice(-2)).join('');
// }
