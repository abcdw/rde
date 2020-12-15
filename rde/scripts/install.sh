mkdir ~/work
cd ~/work

git clone https://github.com/abcdw/rde

git config --global user.email "andrew@trop.in"
git config --global user.name "Andrew Tropin"

ln -sf /home/guest/work/rde/rde/sway ~/.config/
ln -sf /home/guest/work/rde/stale/dotfiles/.config/alacritty ~/.config/
ln -sf /home/guest/work/rde/stale/dotfiles/.tmux.conf ~/

sudo -s
cryptsetup luksOpen /dev/nvme0n1p5 enc
mount -t btrfs /dev/mapper/enc /mnt



btrfs subvolume create /mnt/root
btrfs subvolume create /mnt/boot
btrfs subvolume create /mnt/home
btrfs subvolume create /mnt/gnu
btrfs subvolume create /mnt/data
btrfs subvolume create /mnt/log

btrfs subvolume snapshot -r /mnt/root /mnt/root-blank

umount /mnt


mount -o subvol=root /dev/mapper/enc /mnt

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
