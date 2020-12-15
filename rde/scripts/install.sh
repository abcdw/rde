mkdir ~/work
cd ~/work
git clone https://github.com/abcdw/rde

git config --global user.email "andrew@trop.in"
git config --global user.name "Andrew Tropin"

sudo -s
cd /mnt
mkdir home
mkdir gnu
mkdir data
mkdir var/log -p
mkdir boot

mount -o subvol=home /dev/mapper/enc home
mount -o subvol=gnu /dev/mapper/enc gnu
mount -o subvol=data /dev/mapper/enc data
mount -o subvol=log /dev/mapper/enc var/log
mount -o subvol=boot /dev/mapper/enc boot

herd start cow-store /mnt

cp /etc/configuration/desktop.scm /home/guest/work/rde/rde/system/
cd /home/guest/work/rde/rde/system/
chown guest:users desktop.scm
chmod u+w desktop.scm

cryptsetup luksUUID /dev/nvme0n1p5
