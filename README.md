## Purpose

A way to get a Clojure development environment based on Vagrant.

### What do you get ###
* Fully configured Emacs editor
* All dependencies already installed

## Getting started

### VirtualBox ###
Install the latest version of [VirtualBox](https://www.virtualbox.org/wiki/Downloads).

### Vagrant ###
Install the latest version of [Vagrant](https://www.vagrantup.com/downloads.html).

Once it is done, install the vagrant vbguest plugin. The vbguest plugin will ensure the Vagrant and VirtualBox versions are always kept in sync. Install the vagrant vbguest plugin by executing this command ```vagrant plugin install vagrant-vbguest```

## First time usage

### Configuration ###
* Make a copy of the config.yml.template and call it config.yml
* Fill up the config.yml with relevant information like your name, email address, location of private & public keys to be used for working with the source code

### Provisioning ###
Open up a terminal window and navigate to where you have checked out this code ```vagrant up --provision```

This will pull down the box from Vagrant Atlas and prepare it for initial use. This will take a while, so grab a cup of coffee/tea/your favorite beverage while vagrant is doing its work.

Once the control comes back to you, you can then say ```vagrant ssh```. You are now ready to go !

## Regular usage
Open up a terminal window and navigate to where you have checked out this code ```vagrant up```.

This will launch the VM and give the control back to you.

You can then say ```vagrant ssh``` and start your work.

Happy coding !!!
