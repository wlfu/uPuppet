#!/bin/bash

echo '****** Installing Puppet ...'
wget https://apt.puppetlabs.com/puppetlabs-release-pc1-xenial.deb
sudo dpkg -i puppetlabs-release-pc1-xenial.deb
sudo apt update
sudo apt upgrade
sudo apt-get install -y puppetserver

echo '****** Installing Haskell ...'
# See: https://www.haskell.org/downloads/linux
sudo apt-get update || exit 1
sudo apt-get install -y software-properties-common || exit 1
sudo add-apt-repository -y ppa:hvr/ghc || exit 1
sudo apt-get update || exit 1
sudo apt-get install -y cabal-install-1.22 ghc-7.10.3 || exit 1
sudo bash <<EOFSH || exit 1
cat >/etc/profile.d/ghc.sh <<'EOF' || exit 1
# Add ghc & cabal to the PATH
# this file created by setup.sh for the vagrant Puppet image
# see: https://www.haskell.org/downloads/linux
if ! echo "\$PATH" | grep -q /opt/ghc ; then
	export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:\$PATH"
fi
EOF
chmod 0644 /etc/profile.d/ghc.sh || exit 1
EOFSH
. /etc/profile.d/ghc.sh

echo '****** Installing Haskell Modules ...'
sudo /bin/sh <<EOF || exit 1
. /etc/profile.d/ghc.sh
cabal update || exit 1
cabal install MissingH || exit 1
cabal install Aeson || exit 1
cabal install process || exit 1
cabal install filepath || exit 1
cabal install directory || exit 1
EOF

echo '****** Installing Make ...'
sudo apt install -y make || exit 1

echo '****** Installing JQ ...'
sudo apt install -y jq || exit 1

echo '****** Building uPuppet ...'
rm -rf uPuppet || exit 1
tar zxf /vagrant/uPuppet.tgz || exit 1
cd uPuppet || exit 1
# we don't want to rebuild Version.hs, so make sure it looks uptodate
test -f Tmp/Version.hs && touch Tmp/Version.hs
make install || exit 1
cd ..

echo '****** Installing Puppet Test Tools ...'
tar zxf /vagrant/uPuppet-tools.tgz || exit 1
cd uPuppet || exit 1
make install-tools || exit 1
cd ..
puppet-compile -i || exit 1
