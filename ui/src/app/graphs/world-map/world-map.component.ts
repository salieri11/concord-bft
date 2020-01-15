/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import {
  Component,
  Input,
  ViewChild,
  OnChanges,
  AfterViewInit,
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  SimpleChanges,
  OnDestroy,
  HostListener
} from '@angular/core';

import geojsonvt from 'geojson-vt';
import { Map, View, Overlay, VectorTile, Collection } from 'ol';
import Feature from 'ol/Feature';
import Point from 'ol/geom/Point';
import { Style, Fill, Stroke, Circle } from 'ol/style';
import { Vector as VectorLayer, VectorTile as VectorTileLayer } from 'ol/layer';
import VectorSource from 'ol/source/Vector';
import VectorTileSource from 'ol/source/VectorTile';
import { fromLonLat } from 'ol/proj';
import Projection from 'ol/proj/Projection';
import Select from 'ol/interaction/Select';
import GeoJSON from 'ol/format/GeoJSON';
import { pointerMove } from 'ol/events/condition';
import { getCenter } from 'ol/extent';
import { defaults as interactionDefaults } from 'ol/interaction';
import { unByKey } from 'ol/Observable';
import { easeOut } from 'ol/easing';
import { addCommon as addCommonProjections } from 'ol/proj.js';

import { NodeProperties } from './world-map.model';
import { VmwClarityThemeService } from './../../shared/theme.provider';

import * as WorldData from './countries-110m.json';

@Component({
  selector: 'concord-world-map',
  templateUrl: './world-map.component.html',
  styleUrls: ['./world-map.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class WorldMapComponent implements AfterViewInit, OnChanges, OnDestroy {
  // Takes GeoJSON FeatureCollection of Points as input.  Each feature should have properties that conform to NodeProperties.
  @Input('features') features = [];

  @ViewChild('mapContainer', { static: true }) mapContainer;
  @ViewChild('tooltipContainer', { static: true }) tooltipContainer;

  // The map and its map's tooltip/overlay layer
  private map: Map;
  private overlay: Overlay;
  private view: View;
  private vectorSource: VectorSource;

  // The pulse animation duration and its interval
  private animationDuration = 3000;
  private animationInterval;

  // The current node properties used to generate the overlay/tooltip
  nodeProperties: NodeProperties;

  // An observable collection of features for the map
  private featureCollection = new Collection();
  noLocationAvailable: boolean = false;

  private theme: any;

  constructor(
    private ref: ChangeDetectorRef,
    private themeService: VmwClarityThemeService,
  ) {
    this.setTheme(this.themeService.theme);
  }

  ngAfterViewInit() {
    // This is a patch for an angular build issue
    // https://github.com/openlayers/openlayers/issues/9019#issuecomment-444441291
    addCommonProjections();
    this.initMap();

    this.themeService.themeChange.subscribe(theme => {
      this.setTheme(theme);
    });
  }

  ngOnChanges(changes: SimpleChanges): void {

    if (changes.features) {
      this.featureCollection.clear();
      this.noLocationAvailable = false;
      changes.features.currentValue.forEach(cluster => {
        if (cluster.geo && cluster.geo[0] === 0 && cluster.geo[1] === 0) {
          this.noLocationAvailable = true;

          console.log('no location');
        }
        const feature = new Feature(new Point(fromLonLat(cluster.geo)));
        feature.setProperties(cluster);
        this.featureCollection.push(feature);
      });

      if (changes.features.previousValue && changes.features.previousValue.length === 0) {
        this.redrawMap();
      }
    }
  }

  ngOnDestroy(): void {
    if (this.animationInterval) {
      clearInterval(this.animationInterval);
    }
  }

  @HostListener('window:resize', ['$event'])
  onResize() {
    this.viewFit();
  }

  private initMap() {
    const tilePixels = new Projection({
      code: 'TILE_PIXELS',
      units: 'tile-pixels'
    });

    // Styles for the country layer - same fill and stroke for flat earth look
    const countryStyle = new Style({
      fill: new Fill({
        color: this.theme.countryFill
      }),
      stroke: new Stroke({
        color: this.theme.countryBorder,
        width: 1
      })
    });
    // Fill color for feature bubbles
    const nodeFeatureFill = new Fill({ color: this.theme.nodeFill });
    const nodeFeatureFillSelected = new Fill({ color: this.theme.nodeFillSelected });

    // Overlay container for the tooltip on node hover
    this.overlay = new Overlay({
      element: this.tooltipContainer.nativeElement,
      positioning: 'right',
      autoPan: true,
      autoPanAnimation: {
        source: null,
        duration: 500
      }
    });

    this.vectorSource = new VectorSource({
      wrapX: false,
      features: this.featureCollection
    });
    // A layer to hold the nodes
    const nodeFeatureLayer = new VectorLayer({
      source: this.vectorSource,
      style: this.nodeFeatureStyle(nodeFeatureFill)
    });

    // Set up a hover interaction with the node layer to show the overlay/tooltip
    const nodeFeatureHoverInteraction = new Select({
      wrapX: false,
      layers: [nodeFeatureLayer],
      condition: pointerMove,
      style: this.nodeFeatureStyle(nodeFeatureFillSelected)
    });

    nodeFeatureHoverInteraction.on('select', (event: any) => {
      if (event.selected.length > 0) {
        // On hover of a node, store the data from the GeoJSON properties object and show the overlay popup
        const feature = event.selected[0];
        this.nodeProperties = feature.getProperties();
        const extent = event.selected[0].getGeometry().getExtent();
        const center = getCenter(extent);
        this.overlay.setPosition(center);
      }
      this.ref.markForCheck();
    });

    this.view = new View();
    this.map = new Map({
      layers: [nodeFeatureLayer],
      overlays: [this.overlay],
      target: this.mapContainer.nativeElement,
      view: this.view,
      interactions: interactionDefaults({ mouseWheelZoom: false })
    });
    this.map.addInteraction(nodeFeatureHoverInteraction);
    // Set up pulsing animation on map features
    this.checkAndSchedulePulseAnimation();
    this.animationInterval = setInterval(this.checkAndSchedulePulseAnimation.bind(this), this.animationDuration + 1000);

    // Testing is throwing errors when we don't wild card import
    // So for dev server to work we need to get default attribute of
    // import
    let worldData = WorldData;
    if (WorldData.default) {
      worldData = WorldData.default;
    }
    // Convert GeoJSON source to vector tiles
      const tileSource = geojsonvt(worldData, {
        extent: 4096,
        debug: 0
      });

      // A layer to hold the world map
      const countryOutlineLayer = new VectorTileLayer({
        source: new VectorTileSource({
          format: new GeoJSON(),
          wrapX: false,
          tileLoadFunction: function(tile: VectorTile) {
            const format = tile.getFormat();
            const tileCoord = tile.getTileCoord();
            // Grab the desired tile from the geojsonvt vector tile source
            const data = tileSource.getTile(tileCoord[0], tileCoord[1], -tileCoord[2] - 1);
            // Convert it to GeoGSON
            const features = format.readFeatures({
              type: 'FeatureCollection',
              features: (data.features || []).map(vectorTileFeatureToGeoJsonFeature)
            });
            tile.setLoader(function() {
              tile.setFeatures(features);
              tile.setProjection(tilePixels);
            });
          },
          url: 'data:', // Fake a URL, otherwise tileLoadFunction won't be called
          projection: null
        }),
        style: countryStyle
      });

      // Insert this layer behind all other layers
      this.map.getLayers().insertAt(0, countryOutlineLayer);
      this.map.updateSize();
      this.viewFit();
  }

  /**
   * Unset various properties to hide the active hovered tooltip/overlay
   */
  onLeaveTooltip() {
    this.nodeProperties = undefined;
    this.overlay.setPosition(undefined);
  }


  private viewFit() {
    if (this.features && this.features.length === 0) {
      this.view.setCenter([0, 0]);
      this.view.setZoom(1.1);
    } else if (this.features && this.features.length === 1) {
      this.view.setCenter(fromLonLat(this.features[0].geo));
      this.view.setZoom(3);
    } else {
      this.view.fit(
        this.vectorSource.getExtent(),
        { padding: [50, 80, 50, 50], constrainResolution: false }
      );
    }
  }
  /**
   * Checks for any deploying nodes and schedules their animation. Meant to be called on an interval.
   */
  private checkAndSchedulePulseAnimation() {
    this.featureCollection.forEach(feature => {
      const isDeploying = (feature.getProperties() as NodeProperties).nodes.filter(node => node.status === 'Deploying');
      if (isDeploying.length > 0) {
        this.startPulseAnimation(feature);
      }
    });
  }

  /**
   * Performs a pulsing animation on a single node feature
   */
  private startPulseAnimation(feature) {
    const start = new Date().getTime();
    const listenerKey = this.map.on('postcompose', (event) => {
      const minOpacity = 0;
      const maxOpacity = 255;
      const vectorContext = event.vectorContext;
      const frameState = event.frameState;
      const flashGeom = feature.getGeometry().clone();
      const elapsed = frameState.time - start;
      const elapsedRatio = elapsed / this.animationDuration;
      const originalRadius = (feature.getProperties() as NodeProperties).nodes.length * 10;
      const radius = originalRadius + easeOut(elapsedRatio) * originalRadius * 0.5;
      // Get opacity range based on elapsed time and convert to hex
      let opacity = clamp(Math.floor(easeOut(1 - elapsedRatio) * maxOpacity), minOpacity, maxOpacity).toString(16);
      // Opacity in hex format must have 2 characters so pad with 0 if needed;
      if (opacity.length === 1) {
        opacity = `0${opacity}`;
      }
      const style = new Style({
        image: new Circle({
          radius: radius,
          snapToPixel: false,
          stroke: new Stroke({
            color: `${this.theme.nodeAnimation}${opacity}`,
            width: 1
          })
        })
      });

      vectorContext.setStyle(style);
      vectorContext.drawGeometry(flashGeom);
      if (elapsed > this.animationDuration) {
        unByKey(listenerKey);
        return;
      }
      this.map.render();
    });
    this.map.render();
  }

  /**
   * An ol.style.Style generating function. Needs to be generated per node to show difference in size according to how
   * many nodes at the location.
   *
   * @param fill ol.style.Fill A
   * @returns {(feature) => ol.style.Style} A per-node generating style function
   */
  private nodeFeatureStyle(fill) {

    return (feature) => {
      const unhealthyNodes = (feature.getProperties() as NodeProperties)
        .nodes.filter(node => node.status === 'unhealthy').length > 0;

      if (unhealthyNodes) {
        fill.color_ = this.theme.unhealthyNode;
      } else {
        fill.color_ = this.theme.nodeFill;
      }

      return new Style({
        image: new Circle({
          fill,
          stroke: unhealthyNodes ? new Stroke({
            color: this.theme.unhealthyNode,
            width: 3
          }) : null,
          radius: 5
        })
      });
    };
  }

  private redrawMap() {
    if (this.map) {
      this.map.setTarget(null);
      this.map = null;
      this.initMap();
    }
  }

  private setTheme(type: string) {
    if (type === 'Light') {
      this.theme = {
        countryFill: '#ECEAE6',
        countryBorder: '#DCDBD8', // #DCDBD8
        nodeFill: '#60B515',
        unhealthyNode: '#F52F22',
        nodeFillSelected: '#00ffff',
        nodeAnimation: '#003D79'
      };
    } else {
      this.theme = {
        countryFill: '#204454',
        countryBorder: '#204454',
        nodeFill: '#00ffffaa',
        unhealthyNode: '#e62700aa',
        nodeFillSelected: '#00ffff',
        nodeAnimation: '#00ffff'
      };
    }

    this.redrawMap();
  }
}

/**
 * Clamp a given number between two given ranges
 *
 * @param {number} value The value to clamp
 * @param {number} min The smallest value
 * @param {number} max The largest value
 * @returns {number} A value within the range of min and max
 */
function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

/**
 * Convert a VectorTile feature to a GeoJSON feature
 * Adapted from https://openlayers.org/en/latest/examples/geojson-vt.html
 *
 * @param feature A VectorTile feature
 * @returns a GeoJSON feature
 */

function vectorTileFeatureToGeoJsonFeature(feature) {
  let type;
  let coordinates = feature.geometry;

  if (feature.type === 1) {
    type = 'MultiPoint';
    if (coordinates.length === 1) {
      type = 'Point';
      coordinates = coordinates[0];
    }
  } else if (feature.type === 2) {
    type = 'MultiLineString';
    if (coordinates.length === 1) {
      type = 'LineString';
      coordinates = coordinates[0];
    }
  } else if (feature.type === 3) {
    type = 'Polygon';
    if (coordinates.length > 1) {
      type = 'MultiPolygon';
      coordinates = [coordinates];
    }
  }

  return {
    type: 'Feature',
    geometry: {
      type,
      coordinates
    }
  };
}
