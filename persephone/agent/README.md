# Lifecycle - Agent

This Blockchain sub-project is a service to monitor, upgrade the
Concord-replicas.

## Building

Please see [../README.md](README.md) in the parent directory for how
to build docker images. The rest of this section explains how to build
Lifecycle-agent natively, which can be useful for debugging.

Agent depends on the communication module, so you should have built
(../communication) before this.

Agent is built using mvn:

```
mvn clean package
```
