// Copyright 2018 VMware, all rights reserved

#ifndef UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#define UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#include <prometheus/counter.h>
#include <prometheus/exposer.h>
#include <prometheus/registry.h>
#include <string>
#include <utility>
#include "Metrics.hpp"

namespace concord::utils {
class PrometheusRegistry {
  prometheus::Exposer exposer_;

 public:
  explicit PrometheusRegistry(const std::string& bindAddress)
      : exposer_(bindAddress, "/metrics", 1){};
  void scrapeRegistry(std::shared_ptr<prometheus::Collectable> registry) {
    exposer_.RegisterCollectable(registry);
  }
};

class ConcordBftPrometheusCollector : public prometheus::Collectable {
  typedef std::tuple<std::vector<std::string>, std::vector<std::string>,
                     std::vector<std::string>>
      MetricsNamesByType;

 public:
  typedef std::map<std::string, MetricsNamesByType> MetricsConfiguration;

 private:
  MetricsConfiguration metrics_to_export_;
  std::shared_ptr<concordMetrics::Aggregator> aggregator_;
  prometheus::ClientMetric collect(const std::string& componentName,
                                   const std::string& name,
                                   concordMetrics::Counter c) const;
  prometheus::ClientMetric collect(const std::string& componentName,
                                   const std::string& name,
                                   concordMetrics::Gauge g) const;
  prometheus::ClientMetric collect(const std::string& componentName,
                                   const std::string& name,
                                   concordMetrics::Status s) const;
  std::vector<prometheus::MetricFamily> collectCounters(
      const std::string& componentName,
      const std::vector<std::string>& countersNames);
  std::vector<prometheus::MetricFamily> collectGauges(
      const std::string& componentName,
      const std::vector<std::string>& gaugesNames);
  std::vector<prometheus::MetricFamily> collectStatuses(
      const std::string& componentName,
      const std::vector<std::string>& statusesNames);

 public:
  ConcordBftPrometheusCollector(const MetricsConfiguration& metrics_to_export);
  std::shared_ptr<concordMetrics::Aggregator> getAggregator();
  std::vector<prometheus::MetricFamily> Collect() override;

  static MetricsConfiguration parseConfiguration(const std::string& confPath);
};
}  // namespace concord::utils
#endif  // UTILS_CONCORD_PROMETHEUS_METRICS_HPP
