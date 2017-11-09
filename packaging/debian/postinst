#!/bin/sh
#
#
chmod -R 755 /opt/focus
#
ln -sf /opt/focus/bin/focus /usr/bin/focus
#
envfile=/etc/profile.d/focus.sh
if [ -e ${envfile} ]; then
    rm ${envfile}
fi
echo "# Added by FOCUS on `date`:" >> ${envfile}
echo "# (makes standalone programs and Python modules available)" >> ${envfile}
echo "export FOCUS_ROOT=${PREFIX}" >> ${envfile}
echo "export PATH=\${FOCUS_ROOT}/bin:\${PATH}" >> ${envfile}
echo "export PATH=\${FOCUS_ROOT}/kernel/mrc/bin:\${PATH}" >> ${envfile}
echo "export PATH=\${FOCUS_ROOT}/scripts/proc:\${PATH}" >> ${envfile}
echo "export PYTHONPATH=\${FOCUS_ROOT}/scripts/proc:\${PYTHONPATH}" >> ${envfile}
chmod 755 ${envfile}
#
exit 0