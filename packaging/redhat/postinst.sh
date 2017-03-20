#!/bin/sh
#
#
chmod -R 755 /opt/focus
#
ln -sf /opt/focus/bin/focus /usr/bin/focus
#
if [ -e /etc/profile.d/focus.sh ]; then
    rm /etc/profile.d/focus.sh
fi
echo "# Focus environment variables" >> /etc/profile.d/focus.sh
echo "export PYTHONPATH=/opt/focus/scripts/proc:${PYTHONPATH}" >> /etc/profile.d/focus.sh
echo "export PATH=/opt/focus/scripts/proc:${PATH}" >> /etc/profile.d/focus.sh
chmod 755 /etc/profile.d/focus.sh
#
exit 0
