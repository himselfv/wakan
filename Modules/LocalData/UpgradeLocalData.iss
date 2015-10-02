# If there's already Wakan installed in the target folder, we may have to move LocalData
# from where older versions stored it.

# Setup includes this and runs upgradeLocalData(targetFolder). Add any checks
# as function calls there, mind call order (older upgrades go first).

# Q: Why not do this in Wakan, on launch?
# A: It will slow down loading.
#    It might be a lot of checks.
#    It needs to run with administrator priveleges (setup naturally runs so).
#    Automatic intervention may frustrate the user (at least setup is run intentionally).

# Some compability is integrated into Wakan though, where the price is cheap.

function UpgradeLocalData(targetFolder: string);
begin

end;

