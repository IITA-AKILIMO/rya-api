# Rapid-yield-assessment tool (RYA) API

## Turning into a service

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
ExecStart=/usr/bin/env Rscript /path/to/server.R

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