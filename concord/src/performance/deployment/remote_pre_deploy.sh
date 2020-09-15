sudo umount /media/concord
if mountpoint -q -- /media; then
  echo "SSD is mounted"
else
  echo "creating filesystem and mountingn SSD..."
  sudo mkdir /media
  sudo mkfs -t ext4 -F /dev/nvme0n1
  sudo mount /dev/nvme0n1 /media
  mkdir /media/concord
fi

echo "installing dependencies..."
sudo apt-get update
sudo apt-get -y install docker docker-compose pass gnupg2 gdb
sudo docker login -u DOCKER_USERNAME -p DOCKER_TOKEN DOCKER_SERVER
sudo docker system prune -f
exit 0
