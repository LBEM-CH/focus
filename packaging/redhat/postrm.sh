#!/bin/sh
#
#

# Remove old 2dx if present
rm -rf /opt/2dx
rm -rf /usr/bin/2dx
rm -f /usr/share/applications/2dx.desktop
rm -f /.local/share/applications/2dx.desktop

rm -rf /usr/bin/2dx_image
rm -f /usr/share/applications/2dx_image.desktop

rm -rf /usr/bin/2dx_logbrowser
rm -f /usr/share/applications/2dx_logbrowser.desktop

# Remove old focus if present
rm -rf /opt/focus
rm -rf /usr/bin/focus
rm -f /.local/share/applications/focus.desktop

exit 0