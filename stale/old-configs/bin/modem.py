#!/usr/bin/python2
#script from habr

import binascii
import sys

data=''
err="Usage: python ussd.py action (code)\r\nActions: balans, popolnit, data-status, 3g-data-status, signal"
if len(sys.argv) < 2:
    print err
    sys.exit()
f = open("/dev/ttyACM1", "r+")

if sys.argv[1] == 'balans':
    print>>f, "AT+CUSD=1,AA180C3602,15\r\n"
    while data[:5]!="+CUSD":
        data=f.readline()
    print data
    data = data[10:-6]

    print binascii.unhexlify(data)
elif sys.argv[1] == 'signal':
    print>>f, "AT+CSQ\r\n"
    while data[:5]!="+CSQ:":
        data=f.readline()
    data = data[6:].replace(',', '.')
    print data
    sig_str = -113+float(data)*2
    sig_per = float(data)*100 / 31
    print unicode(sig_str)+"dBm \n"+unicode(int(sig_per))+"%"
elif sys.argv[1] == 'popolnit':
    print>>f, "AT+CUSD=1,*123*"+sys.argv[2]+"#,15\r\n"
    while data[:5]!="+CUSD":
        data=f.readline()
    data = data[10:-6]
    print binascii.unhexlify(data)
elif sys.argv[1] == 'data-status':
    print>>f, "AT+CUSD=1,*121#,15\r\n"
    while data[:5]!="+CUSD":
        data=f.readline()
    data = data[10:-6]
    print binascii.unhexlify(data)
elif sys.argv[1] == '3g-data-status':
    print>>f, "AT+CUSD=1,*122#,15\r\n"
    while data[:5]!="+CUSD":
        data=f.readline()
    data = data[10:-6]
    print binascii.unhexlify(data)
else:
    print error
f.close
