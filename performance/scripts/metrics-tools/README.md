Metrics extraction from logs

Goal: 

A generic tool to extract metrics from docker logs
and present in readable format.

How to use:  

- Update extract_metrics_conf.yml as required. 
- RUN 'python extract_metrics.py'
- A data.json will be created as result file.


Limitation:

- To keep the tool generic, we don't support calculation of
  result based on metrics. All result calculation should be
  done in component logs.
- Only one metrics can extrcated per <metric name>



