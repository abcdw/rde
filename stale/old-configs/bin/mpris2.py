#!/usr/bin/env python2

import sys
import dbus
import getopt

try:
    opts, args = getopt.getopt(sys.argv[1:], "h", ["help"])
except getopt.error, msg:
    print msg
    print "use --help for help"
    sys.exit(2)
    
help_text = "usage: ./mpris2.py [play|pause|stop|toggleplay|next|prev|nowplaying]"
for o, a in opts:
    if o in ("-h", "--help"):
        print help_text
        sys.exit(0)

bus = dbus.SessionBus()

dbusNamePrefix = 'org.mpris.MediaPlayer2.'
target = None
dbusObj = bus.get_object('org.freedesktop.DBus', '/')
for name in dbusObj.ListNames(dbus_interface='org.freedesktop.DBus'):
    if name.startswith(dbusNamePrefix):
        print("Found media player: %s" % name[len(dbusNamePrefix):])
        target = name
        break

assert target is not None

targetObject = bus.get_object(target, '/org/mpris/MediaPlayer2')
mpris = dbus.Interface(targetObject, dbus_interface='org.mpris.MediaPlayer2.Player')
properties = dbus.Interface(targetObject, dbus_interface='org.freedesktop.DBus.Properties')

cmd = None
if len(sys.argv) > 1:
    cmd = sys.argv[1]
if len(sys.argv) > 2:
    args = sys.argv[2:]

if cmd == 'toggleplay':
    mpris.PlayPause()
elif cmd == 'play':
    mpris.Play()
elif cmd == 'pause':
    mpris.Pause()
elif cmd == 'stop':
    mpris.Stop()
elif cmd == 'next':
    mpris.Next()
elif cmd == 'prev':
    mpris.Previous()
elif cmd == 'nowplaying':
    props = properties.GetAll('org.mpris.MediaPlayer2.Player')
    props = dict(**props)
    metadata = props['Metadata']
    print("Now playing %s by %s from %s at pos %i" % (metadata["xesam:title"], metadata["xesam:artist"], metadata["xesam:album"], props["Position"]))
