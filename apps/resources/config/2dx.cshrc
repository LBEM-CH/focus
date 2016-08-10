# In case this does not exist:
if ( -e /usr/local/ccp4/bin/ccp4.setup-csh ) then
  echo "# sourcing: /usr/local/ccp4/bin/ccp4.setup-csh"
  source "/usr/local/ccp4/bin/ccp4.setup-csh"
else
  echo "# WARNING:"
  echo "# WARNING: /usr/local/ccp4/bin/ccp4.setup-csh does not exist."
  echo "# WARNING: /usr/local/ccp4/bin/ccp4.setup-csh does not exist."
  echo "# WARNING: /usr/local/ccp4/bin/ccp4.setup-csh does not exist."
  echo "# WARNING:"
endif
setenv DYLD_LIBRARY_PATH "/usr/local/2dx/lib:$DYLD_LIBRARY_PATH"
