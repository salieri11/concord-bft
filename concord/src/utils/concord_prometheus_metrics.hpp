// Copyright 2018 VMware, all rights reserved

#ifndef UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#define UTILS_CONCORD_PROMETHEUS_METRICS_HPP
#include <prometheus/counter.h>
#include <prometheus/exposer.h>
#include <prometheus/gauge.h>
#include <prometheus/histogram.h>
#include <prometheus/registry.h>
#include <prometheus/summary.h>
#include <string>
#include <utility>
#include "Logger.hpp"
#include "Metrics.hpp"
#include "Statistics.hpp"

namespace concord::utils {
class PrometheusRegistry;
template <typename T>
class ConcordCustomCollector : public prometheus::Collectable {
  logging::Logger logger_;
  std::vector<std::shared_ptr<prometheus::Family<T>>> metrics_;
  std::chrono::seconds dumpInterval_;
  std::chrono::seconds last_dump_time_;
  std::mutex lock_;
  prometheus::Family<T>& createFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

 public:
  ConcordCustomCollector(std::chrono::seconds dumpInterval)
      : logger_(logging::getLogger("com.vmware.concord.prometheus")),
        dumpInterval_(dumpInterval),
        last_dump_time_(0) {}
  std::vector<prometheus::MetricFamily> Collect() override;
  friend class PrometheusRegistry;
};

class PrometheusRegistry {
  prometheus::Exposer exposer_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Counter>>
      counters_custom_collector_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Gauge>>
      gauges_custom_collector_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Histogram>>
      histogram_custom_collector_;
  std::shared_ptr<ConcordCustomCollector<prometheus::Summary>>
      summary_custom_collector_;

 public:
  explicit PrometheusRegistry(
      const std::string& bindAddress,
      uint64_t metricsDumpInterval /* 10 minutes by default */);

  explicit PrometheusRegistry(const std::string& bindAddress);

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

  prometheus::Family<prometheus::Histogram>& createHistogramFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  // Measures distribution of quantity such as duration.
  // User defines a list of buckets, and calls Observe
  // With the observed quantity.
  // As a result, the counter of the corresponding bucket is being incremented.
  prometheus::Histogram& createHistogram(
      prometheus::Family<prometheus::Histogram>& source,
      const std::map<std::string, std::string>& labels,
      const std::vector<double>& buckets);

  prometheus::Histogram& createHistogram(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels,
      const std::vector<double>& buckets);

  prometheus::Family<prometheus::Summary>& createSummaryFamily(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels);

  prometheus::Summary& createSummary(
      prometheus::Family<prometheus::Summary>& source,
      const std::map<std::string, std::string>& labels,
      const prometheus::Summary::Quantiles& quantiles,
      std::chrono::milliseconds max_age = std::chrono::seconds{60},
      int age_buckets = 5);

  prometheus::Summary& createSummary(
      const std::string& name, const std::string& help,
      const std::map<std::string, std::string>& labels,
      const prometheus::Summary::Quantiles& quantiles,
      std::chrono::milliseconds max_age = std::chrono::seconds{60},
      int age_buckets = 5);

 private:
  static const uint64_t defaultMetricsDumpInterval = 600;
};

class ConcordBftPrometheusCollector : public prometheus::Collectable {
  const std::string metricNamePrefix_ = "concord_concordbft_";
  std::shared_ptr<concordMetrics::Aggregator> aggregator_;
  prometheus::ClientMetric collect(const std::string& component,
                                   concordMetrics::Counter& c) const;
  prometheus::ClientMetric collect(const std::string& component,
                                   concordMetrics::Gauge& g) const;
  prometheus::ClientMetric collect(const std::string& component,
                                   concordMetrics::Status& s) const;

  std::vector<prometheus::MetricFamily> collectCounters();

  std::vector<prometheus::MetricFamily> collectGauges();

  std::vector<prometheus::MetricFamily> collectStatuses();

  std::string getMetricName(const std::string& origName);

 public:
  ConcordBftPrometheusCollector()
      : aggregator_(std::make_shared<concordMetrics::Aggregator>()) {}
  std::vector<prometheus::MetricFamily> Collect() override;
  std::shared_ptr<concordMetrics::Aggregator> getAggregator() {
    return aggregator_;
  }
};

class ConcordBftStatisticsCollector;
class ConcordBftStatisticsFactory : public concordMetrics::IStatisticsFactory {
 public:
  class ConcordBftSummaryImp : public concordMetrics::ISummary {
    prometheus::Summary& summary_;

   public:
    explicit ConcordBftSummaryImp(prometheus::Summary& summary)
        : summary_(summary) {}
    void Observe(double value) override { summary_.Observe(value); };
    concordMetrics::SummaryDescription Collect() override { return {}; };
  };

  ConcordBftStatisticsFactory()
      : concordBFtSummaries_("concordBftSummaries",
                             "Collection of all concordBftSummaries", {}) {}
  std::unique_ptr<concordMetrics::ISummary> createSummary(
      const std::string& name,
      const concordMetrics::Quantiles& quantiles) override {
    prometheus::Summary::Quantiles q;
    for (auto& quantile : quantiles) {
      q.emplace_back(prometheus::detail::CKMSQuantiles::Quantile(
          quantile.quantile, quantile.value));
    }
    return std::make_unique<ConcordBftSummaryImp>(
        concordBFtSummaries_.Add({{"internalMetricName", name}}, q));
  }

 private:
  prometheus::Family<prometheus::Summary> concordBFtSummaries_;
  friend ConcordBftStatisticsCollector;
};

class ConcordBftStatisticsCollector : public prometheus::Collectable {
  concordMetrics::IStatisticsFactory& bftStatisticsFactory_;

 public:
  ConcordBftStatisticsCollector()
      : bftStatisticsFactory_(concordMetrics::StatisticsFactory::get().setImp(
            std::make_unique<ConcordBftStatisticsFactory>())) {}
  std::vector<prometheus::MetricFamily> Collect() override {
    return dynamic_cast<ConcordBftStatisticsFactory&>(bftStatisticsFactory_)
        .concordBFtSummaries_.Collect();
  }
};
}  // namespace concord::utils
#endif  // UTILS_CONCORD_PROMETHEUS_METRICS_HPP
