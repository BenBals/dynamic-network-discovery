project_root=$(git rev-parse --show-toplevel)
local_data_dir="$project_root/data/"
remote_data_dir="/hpi/fs00/home/ben.bals/master-thesis-experiments/data/"

rsync \
  --compress \
  --verbose \
  --human-readable \
  --partial \
  --progress \
  --recursive \
  summon.ssh:$remote_data_dir "$local_data_dir"