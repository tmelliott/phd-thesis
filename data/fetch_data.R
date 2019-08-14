# fetch historical data from raspberry pi
date <- "2019-08-13" # monday, august 13 2019
pi <- Sys.getenv("pi_ip")

hfile <-
    file.path(
        '/mnt', 'storage', 'history',
        format(as.Date(date), '%Y/%m/%d'),
        paste0(
            'archive_',
            format(as.Date(date), '%Y_%m_%d'),
            '.zip'
        )
    )

system(sprintf('scp tom@%s:%s archive.zip', pi, hfile))
