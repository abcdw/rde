#!/bin/bash

prefix=/usr/local/freeswitch
srcdir=/usr/local/src
user=`whoami`
group=`id -g -n`

echo Installing dependices...
sudo apt-get install git-core build-essential autoconf automake libtool \
    libncurses5 libncurses5-dev make libjpeg-dev pkg-config unixodbc \
    unixodbc-dev zlib1g-dev libcurl4-openssl-dev libexpat1-dev libssl-dev \
    libtiff4-dev libx11-dev python-dev python2.6-dev libvorbis-dev bison \
    libzrtpcpp-dev libasound2-dev libogg-dev  libperl-dev libgdbm-dev \
    libdb-dev uuid-dev

echo Downloading sources...
mkdir -p $srcdir
cd $srcdir
sudo rm freeswitch -rf
sudo mkdir freeswitch
sudo chown $user:$group freeswitch
git clone git://git.freeswitch.org/freeswitch.git
cd freeswitch

echo Configuring sources...
./bootstrap.sh
sed -i '/codec2/s/^#//g' ./modules.conf
./configure --prefix=$prefix

echo Building sources...
make
echo Installing freeswitch...
sudo make all install cd-sounds-install cd-moh-install

sudo sed -i '/Codec/a\    <load module="mod_codec2"/>' \
    $prefix/conf/autoload_configs/modules.conf.xml
sudo sed -i 's/GSM/GSM,CODEC2/' $prefix/conf/vars.xml

# /usr/local/freeswitch/bin/freeswitch
