// Copyright 2018 VMware, all rights reserved

#include "concord_prometheus_metrics.hpp"
#include <algorithm>
#include <tuple>
#include "yaml-cpp/yaml.h"

using namespace prometheus;
using namespace concordMetrics;

namespace concord::utils {

ClientMetric ConcordBftPrometheusCollector::collect(
    const std::string& componentName, const std::string& name,
    concordMetrics::Counter c) const {
  ClientMetric metric;
  metric.counter.value = c.Get();
  metric.label = {{"source", "concordbft"}, {"component", componentName}};
  return metric;
}

ClientMetric ConcordBftPrometheusCollector::collect(
    const std::string& componentName, const std::string& name,
    concordMetrics::Gauge g) const {
  ClientMetric metric;
  metric.gauge.value = g.Get();
  metric.label = {{"source", "concordbft"}, {"component", componentName}};
  return metric;
}

ClientMetric ConcordBftPrometheusCollector::collect(
    const std::string& componentName, const std::string& name,
    concordMetrics::Status s) const {
  // not supported by prometheus
  return ClientMetric();
}

ConcordBftPrometheusCollector::ConcordBftPrometheusCollector(
    const ConcordBftPrometheusCollector::MetricsConfiguration&
        metrics_to_export)
    : metrics_to_export_(metrics_to_export),
      aggregator_(std::make_shared<Aggregator>()) {}
std::shared_ptr<Aggregator> ConcordBftPrometheusCollector::getAggregator() {
  return aggregator_;
}

std::vector<MetricFamily> ConcordBftPrometheusCollector::Collect() {
  auto results = std::vector<MetricFamily>{};
  for (auto& pair : metrics_to_export_) {
    auto counters = collectCounters(pair.first, std::get<0>(pair.second));
    results.insert(results.end(), std::move_iterator(counters.begin()),
                   std::move_iterator(counters.end()));
    auto gauges = collectGauges(pair.first, std::get<1>(pair.second));
    results.insert(results.end(), std::move_iterator(gauges.begin()),
                   std::move_iterator(gauges.end()));
    auto statuses = collectStatuses(pair.first, std::get<2>(pair.second));
    results.insert(results.end(), std::move_iterator(statuses.begin()),
                   std::move_iterator(statuses.end()));
  }
  return results;
}

std::vector<MetricFamily> ConcordBftPrometheusCollector::collectCounters(
    const std::string& componentName,
    const std::vector<std::string>& countersNames) {
  std::vector<MetricFamily> cf;
  for (auto& c : countersNames) {
    cf.emplace_back(
        MetricFamily{c,
                     "concord bft " + c + " metric",
                     MetricType::Counter,
                     {collect(componentName, c,
                              aggregator_->GetCounter(componentName, c))}});
  }
  return cf;
}

std::vector<MetricFamily> ConcordBftPrometheusCollector::collectGauges(
    const std::string& componentName,
    const std::vector<std::string>& gaugesNames) {
  std::vector<MetricFamily> gf;
  for (auto& g : gaugesNames) {
    gf.emplace_back(MetricFamily{
        g,
        "concord bft " + g + " metric",
        MetricType::Gauge,
        {collect(componentName, g, aggregator_->GetGauge(componentName, g))}});
  }
  return gf;
}

std::vector<MetricFamily> ConcordBftPrometheusCollector::collectStatuses(
    const std::string& componentName,
    const std::vector<std::string>& statusesNames) {
  // Not supported by prometheus
  return {};
}

ConcordBftPrometheusCollector::MetricsConfiguration
ConcordBftPrometheusCollector::parseConfiguration(const std::string& confPath) {
  YAML::Node config = YAML::LoadFile(confPath);
  MetricsConfiguration confOut;
  for (const auto& component : config["concordbft"]) {
    std::vector<std::string> counters;
    std::vector<std::string> gauges;
    std::vector<std::string> statuses;
    for (const auto& c : component.second["counters"]) {
      counters.emplace_back(c.as<std::string>());
    }
    for (const auto& g : component.second["gauges"]) {
      gauges.emplace_back(g.as<std::string>());
    }
    confOut.emplace(component.first.as<std::string>(),
                    std::make_tuple(std::move(counters), std::move(gauges),
                                    std::move(statuses)));
  }
  return confOut;
}

}  // namespace concord::utils
