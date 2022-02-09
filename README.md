# Rapid-yield-assessment tool (RYA) API

## Turning into a service

Please note that although `systemd` has become the default init system for many Linux distributions, it isn’t implemented universally across all distros. As you go through this tutorial, if your terminal outputs the error `bash: systemctl is not installed` then it is likely that your machine has a different init system installed.

Let’s create a file called `/etc/systemd/system/rya.service`:

``` batch
[Unit]
Description=RYA API service
After=network.target
StartLimitBurst=5  
StartLimitIntervalSec=10

[Service]
Type=simple
Restart=always
RestartSec=30
User=akilimo
ExecStart=/usr/bin/Rscript /home/akilimo/services/rya/server.R

[Install]
WantedBy=multi-user.target
```
You’ll need to:

-   set your actual username after  `User=`
-   set the proper path to your script in  `ExecStart=`

That’s it. We can now start the service:

``` shell
$ systemctl start rya
```
And automatically get it to start on boot:
``` shell
$ systemctl enable rya
```