#!/bin/bash -e

dup uffi -Uftp.med-info.com -D/home/ftp/uffi -C"(cd /opt/apache/htdocs/uffi; make install)" -su $*

