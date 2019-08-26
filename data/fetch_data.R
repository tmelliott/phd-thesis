# fetch historical data from raspberry pi
dates <- paste0("2019-08-", 12:25)
pi <- Sys.getenv("pi_ip")

for (date in dates) {
    cat(" * downloading", date, "...\n")
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
    tfile <- sprintf("archive_%s.zip", format(as.Date(date), "%Y-%m-%d"))
    if (!file.exists(tfile))
        system(sprintf('rsync -avP tom@%s:%s %s', pi, hfile, tfile))
}
cat("\nComplete\n")

# simlink the 13th to "archive"
system("ln -sf archive_2019-08-13.zip archive.zip")
