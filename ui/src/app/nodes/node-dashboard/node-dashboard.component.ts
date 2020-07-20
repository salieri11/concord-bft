/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Component, OnInit, ViewChild, Input, ElementRef } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { WorldMapComponent } from '../../graphs/world-map/world-map.component';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';
import { NodesService } from '../shared/nodes.service';
import { MetricsService } from '../../shared/metrics.service';

import { timeRangesShort } from '../../graphs/wavefront-graph/wavefront-graph.component';

interface GuageData {
  title: string;
  min: number;
  max: number;
  displayValue: number | string;
  data: { name: string; value: number; };
  arrayData?: any[];
  reset: () => void;
  update: () => void;
}

const testIPs = ['52.63.73.141', '54.252.173.111', '52.42.212.121', '44.226.171.161'];

@Component({
  selector: 'concord-node-dashboard',
  templateUrl: './node-dashboard.component.html',
  styleUrls: ['./node-dashboard.component.scss']
})
export class NodeDashboardComponent implements OnInit {
  @Input('replicaId') replicaId: string;

  @ViewChild('nodeSelect', { static: false }) nodeSelect: ElementRef;
  @ViewChild('timeRangeSelect', { static: false }) timeRangeSelect: ElementRef;
  @ViewChild('worldMap', { static: false }) worldMap: WorldMapComponent;

  replicaInfo: any;
  replicaEndpoint: string;
  replicaIP: string;
  nodesInfo: any;
  nodeList: any;
  nodeLocation: any;

  timeRanges = timeRangesShort;
  updateTrigger = 0;
  timeStart = Date.now() - 2 * 3600 * 1000;
  timeEnd = Date.now();

  processes = [];

  graphConfigCPU = {
    title: 'CPU UTILIZATION (%)',
    query: { name: 'cpu', params: { replica_ip: '' } },
    target: { name: 'Service Name', tag: 'bc.service' },
  };
  graphConfigMEM = {
    title: 'MEMORY UTILIZATION (%)',
    query: { name: 'mem', params: { replica_ip: '' } },
    target: { name: 'Service Name', tag: 'bc.service' },
  };
  graphConfigLOG = {
    title: 'LOG SIZE (Bytes)',
    query: { name: 'log', params: { replica_ip: '' } },
    target: { name: 'Service Name', tag: 'bc.service' },
  };
  graphConfigDISK = {
    title: 'DISK UTILIZATIONS (%)',
    query: { name: 'disk', params: { replica_ip: '' } },
    target: { name: 'Service Name', tag: 'bc.service' },
  };

  guages: GuageData[];
  nodeHealth: number = 1;
  nodeColor: string;
  nodeData: { name: string, value: number }[] = [];
  colorScheme = { domain: ['#60B515'] };

  blockchainType: string;

  constructor(
    public blockchainService: BlockchainService,
    private nodesService: NodesService,
    private route: ActivatedRoute,
    private router: Router,
    private metrics: MetricsService,
  ) {
    this.guages = [{
        title: 'CPU',
        min: 0, max: 100,
        displayValue: '_',
        data: { name: 'cpu', value: 10 },
        reset: guageDataReset,
        update: guageDataUpdate,
      }, {
        title: 'Memory',
        min: 0, max: 100,
        displayValue: '_',
        data: { name: 'mem', value: 10 },
        reset: guageDataReset,
        update: guageDataUpdate,
      }, {
        title: 'Disk',
        min: 0, max: 100,
        displayValue: '_',
        data: { name: 'disk', value: 10 },
        reset: guageDataReset,
        update: guageDataUpdate,
      },
    ];
  }

  changeNode() {
    const targetReplicaId = this.nodeSelect.nativeElement.value;
    if (this.replicaId !== targetReplicaId) {
      this.replicaId = targetReplicaId;
      this.router.navigate([`../${targetReplicaId}`], {relativeTo: this.route});
      this.processes = [];
      this.updateNodesInfo();
      ++this.updateTrigger;
    }
  }

  changeTimeRange() {
    const value = parseInt(this.timeRangeSelect.nativeElement.value, 10);
    this.timeStart = Date.now() - value * 1000;
    this.timeEnd = Date.now();
  }

  updateNodeSelect() {
    for (const optionElement of this.nodeSelect.nativeElement.children) {
      if (optionElement.tagName === 'OPTION') {
        if (this.replicaId === optionElement.value) {
          optionElement.setAttribute('selected', 'true');
        }
      }
    }
  }

  updateNodesInfo() {
    if (this.nodesInfo) {
      this.nodeList = this.nodesInfo.nodes;
      let i = 0;
      for (const nodeInfo of this.nodesInfo.nodes) {
        if (this.replicaId === nodeInfo.id) {
          this.replicaInfo = nodeInfo;
          this.replicaEndpoint = nodeInfo.rpc_url.split('://')[1].split('/')[0];
          this.replicaIP = this.replicaEndpoint.split(':')[0];
          this.graphConfigCPU.query.params.replica_ip = testIPs[i];
          this.graphConfigMEM.query.params.replica_ip = testIPs[i];
          this.graphConfigLOG.query.params.replica_ip = testIPs[i];
          this.graphConfigDISK.query.params.replica_ip = testIPs[i];
        }
        ++i;
      }
      this.nodeLocation = null;
      for (const nodeLocation of this.nodesInfo.nodesByLocation) {
        const hasThisReplica = nodeLocation.nodes.filter(item => item.id === this.replicaId);
        if (hasThisReplica) { this.nodeLocation = nodeLocation; break; }
      }
      setTimeout(() => { this.updateNodeSelect(); }, 0);
      for (const guage of this.guages) { guage.reset(); }
    }
  }


  trimValue(value) {
    if (value < 0.01) { return '0.01'; }
    const prec = value < 0.1 ? 1 : 2;
    value = value.toPrecision(prec);
    while (value.endsWith('0')) {
      value = value.substr(0, value.length - 1);
    }
    return value;
  }

  setSeriesData(graphData, propName, postFix, factor = 1) {
    for (const seriesData of graphData) {
      if (seriesData.name === 'VM') { continue; }
      const filter = item => item.name === seriesData.name;
      const currentValue = this.metrics.getCurrentValueOf(graphData, filter);
      const formattedValue = this.trimValue(currentValue / factor);
      this.setProcessProperty(seriesData.name, propName, (formattedValue) + postFix);
    }
  }

  setProcessProperty(procName, propName, propValue) {
    const found = this.processes.filter(proc => proc.name === procName);
    let procData = found[0];
    if (!procData) {
      procData = {
        name: procName
      };
      this.processes.push(procData);
    }
    procData[propName] = propValue;
  }

  onupdateGraphConfigCPU() {
    return (graphData) => {
      const filter = item => item.name === 'VM';
      const currentValue = this.metrics.getCurrentValueOf(graphData, filter);
      const displayValue = currentValue.toPrecision(2) + '%';
      this.guages[0].data.value = currentValue;
      this.guages[0].displayValue = displayValue;
      this.guages[0].update();
      this.setSeriesData(graphData, 'cpu', '%');
    };
  }
  onupdateGraphConfigMEM() {
    return (graphData) => {
      const filter = item => item.name === 'VM';
      const currentValue = this.metrics.getCurrentValueOf(graphData, filter);
      const displayValue = currentValue.toPrecision(2) + '%';
      this.guages[1].data.value = currentValue;
      this.guages[1].displayValue = displayValue;
      this.guages[1].update();
      this.setSeriesData(graphData, 'mem', '%');
    };
  }
  onupdateGraphConfigDISK() {
    return (graphData) => {
      const filter = item => item.name === 'VM';
      const currentValue = this.metrics.getCurrentValueOf(graphData, filter);
      const displayValue = currentValue.toPrecision(2) + '%';
      this.guages[2].data.value = currentValue;
      this.guages[2].displayValue = displayValue;
      this.guages[2].update();
      this.setSeriesData(graphData, 'disk', '%');
    };
  }
  onupdateGraphConfigLOG() {
    return (graphData) => {
      this.setSeriesData(graphData, 'log', ' KiB', 1024);
    };
  }

  ngOnInit() {
    this.nodesInfo = this.nodesService.committersData;
    this.updateNodesInfo();
    ++this.updateTrigger;
  }

  valueFormatting(value) {
    // @ts-ignore
    return `${value}/${this.max}`;
  }

  setNodeColor() {
    if (this.nodeHealth >= .9) {
      this.nodeColor = 'green';
      this.colorScheme.domain[0] = '#60b515';
    } else if (this.nodeHealth >= .7) {
      this.nodeColor = 'yellow';
      this.colorScheme.domain[0] = '#ffdc0b';
    } else if (this.nodeHealth <= .69) {
      this.nodeColor = 'red';
      this.colorScheme.domain[0] = '#c92100';
    }
  }

}


function guageDataReset() { this.displayValue = '_'; this.arrayData = null; }
function guageDataUpdate() { this.arrayData = [{name: this.data.name, value: this.data.value}]; }
