# Build the committer-tools image

```bash
docker build . -f DockerfileCommitterTools -t committer-tools
```

# Manual testing

Load the committer-tools image onto a deployment.

```bash
docker save -o committer-tools.tar committer-tools
scp committer-tools.tar root@<IP>:~/
```

On the remote host, unpack the image and start testing.

```bash
docker load -i committer-tools.tar
eval $(docker run committer-tools)
```
