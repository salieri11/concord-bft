// Copyright 2018 VMware, all rights reserved

#ifndef UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#define UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#include <prometheus/counter.h>
#include <prometheus/exposer.h>
#include <prometheus/gauge.h>
#include <prometheus/registry.h>
#include <string>
#include <utility>
#include "Metrics.hpp"

namespace concord::utils {
struct ConcordMetricConf {
 public:
  std::string name_;
  std::map<std::string, std::string> labels_;
  std::string type_;
  std::string component_;
  std::string description_;
  bool exposed_;
};
class PrometheusRegistry;
class ConcordBftMetricsManager;
template <typename T>
class ConcordCustomCollector : public prometheus::Collectable {
  std::vector<std::shared_ptr<prometheus::Family<T>>> metrics_;
  std::vector<std::shared_ptr<prometheus::Family<T>>> active_metrics_;
  std::mutex lock_;
  prometheus::Family<T>& createFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);
  void activateMetric(const std::string& metricName);
  void deactivateMetric(const std::string& metricName);

 public:
  std::vector<prometheus::MetricFamily> Collect() override;
  friend class PrometheusRegistry;
};

class PrometheusRegistry {
  prometheus::Exposer exposer_;
  std::vector<ConcordMetricConf> metrics_configuration_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Counter>>
      counters_custom_collector_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Gauge>>
      gauges_custom_collector_;

 public:
  explicit PrometheusRegistry(
      const std::string& bindAddress,
      const std::vector<ConcordMetricConf>& metricsConfiguration);

  void scrapeRegistry(std::shared_ptr<prometheus::Collectable> registry);

  prometheus::Family<prometheus::Counter>& createCounterFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  prometheus::Counter& createCounter(
      prometheus::Family<prometheus::Counter>& source,
      const std::map<std::string, std::string>& labels);

  prometheus::Counter& createCounter(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  prometheus::Family<prometheus::Gauge>& createGaugeFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  prometheus::Gauge& createGauge(
      prometheus::Family<prometheus::Gauge>& source,
      const std::map<std::string, std::string>& labels);

  prometheus::Gauge& createGauge(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  static std::vector<ConcordMetricConf> parseConfiguration(
      const std::string& configurationFilePath);
};

class ConcordBftPrometheusCollector : public prometheus::Collectable {
  std::vector<ConcordMetricConf> counters_;
  std::vector<ConcordMetricConf> gauges_;
  std::vector<ConcordMetricConf> statuses_;
  std::shared_ptr<concordMetrics::Aggregator> aggregator_;
  prometheus::ClientMetric collect(const ConcordMetricConf& conf,
                                   concordMetrics::Counter c) const;
  prometheus::ClientMetric collect(const ConcordMetricConf& conf,
                                   concordMetrics::Gauge g) const;
  prometheus::ClientMetric collect(const ConcordMetricConf& conf,
                                   concordMetrics::Status s) const;

  std::vector<prometheus::MetricFamily> collectCounters();

  std::vector<prometheus::MetricFamily> collectGauges();

  std::vector<prometheus::MetricFamily> collectStatuses();

 public:
  ConcordBftPrometheusCollector(
      const std::vector<ConcordMetricConf>& metricsConfiguration,
      std::shared_ptr<concordMetrics::Aggregator> aggregator);
  std::vector<prometheus::MetricFamily> Collect() override;
};

class ConcordBftMetricsManager {
  std::shared_ptr<concordMetrics::Aggregator> aggregator_;
  std::shared_ptr<ConcordBftPrometheusCollector> collector_;

 public:
  ConcordBftMetricsManager(
      const std::vector<ConcordMetricConf>& metricsConfiguration)
      : aggregator_(std::make_shared<concordMetrics::Aggregator>()) {
    std::vector<ConcordMetricConf> exposed_conf;
    std::copy_if(metricsConfiguration.begin(), metricsConfiguration.end(),
                 std::back_inserter(exposed_conf),
                 [](const ConcordMetricConf& c) { return c.exposed_; });
    collector_ = std::make_shared<ConcordBftPrometheusCollector>(exposed_conf,
                                                                 aggregator_);
  }
  std::shared_ptr<ConcordBftPrometheusCollector> getCollector() {
    return collector_;
  }

  std::shared_ptr<concordMetrics::Aggregator> getAggregator() {
    return aggregator_;
  }

  static std::vector<ConcordMetricConf> parseConfiguration(
      const std::string& confPath);
};

}  // namespace concord::utils
#endif  // UTILS_CONCORD_PROMETHEUS_METRICS_HPP
